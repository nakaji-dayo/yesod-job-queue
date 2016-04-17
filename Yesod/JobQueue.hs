{-# LANGUAGE ImpredicativeTypes, UndecidableInstances #-}
module Yesod.JobQueue (
    YesodJobQueue (..)
    , JobQueue
    , getJobQueue
    , JobState
    , JobInfo (..)
    , newJobState) where

import qualified Prelude as P

import Yesod.JobQueue.Routes
import Yesod.JobQueue.APIData

-- import Data.Maybe
import qualified Data.List as L
import ClassyPrelude.Yesod
import Control.Concurrent
import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import Control.Monad.Logger
import System.Cron.Schedule
import qualified Database.Redis as R

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import qualified Control.Concurrent.STM as STM

import Data.Aeson.TH

import Data.FileEmbed (embedFile)

-- persist config for job env
type JobDBConf c = (PersistConfig c) => (c, PersistConfigPool c)

class JobInfo j where
    describe :: j -> String
    describe _ = ""

class (Yesod master, Read (JobType master), Show (JobType master)
      , Enum (JobType master), Bounded (JobType master)
      , JobInfo (JobType master)
      , PersistConfig config)
      => YesodJobQueue master config | master -> config where
    
    -- persistent config for job
    jobDBConfig :: master -> JobDBConf config
    
    -- Custom Job Type
    type JobType master
    
    -- Execute Job Handler
    runJob :: (MonadBaseControl IO m, MonadIO m)
              => master -> JobType master -> ReaderT master m ()
    
    -- queue key name for redis
    queueKey :: master -> ByteString
    queueKey _ = "yesod-job-queue"
    
    -- serialize JobType
    readJobType :: master -> String -> Maybe (JobType master)
    readJobType _ = readMay
    
    -- desserialize JobType
    showJobType :: JobType master -> String
    showJobType = show
    
    -- get all job type list
    allJobTypes :: master -> [JobType master]
    allJobTypes _ = [minBound..]
    -- runDB with config for job
    runDBJob :: (MonadBaseControl IO m, MonadIO m) =>
                PersistConfigBackend config m a -> ReaderT master m a
    runDBJob action = do
        m <- ask
        let (config, pool) = jobDBConfig m
        lift $ runPool config action pool

    -- start thread of dequeue-ing job
    startDequeue :: (MonadBaseControl IO m, MonadIO m) => master -> m ()
    startDequeue m = do
        liftIO $ forkIO $ do
            conn <- R.connect R.defaultConnectInfo
            R.runRedis conn $ do
                forever $ do
                    emr <- R.blpop [queueKey m] 600
                    liftIO $ case emr of
                        Left e -> putStrLn "[dequeue] error in at connection redis"
                        Right Nothing -> putStrLn "[dequeue] timeout retry"
                        Right (Just (k, v)) -> do
                            case readJobType m (BSC.unpack v) of
                             Just jt -> do
                                 job <- RunningJob <$> U.nextRandom
                                 STM.atomically
                                     $ STM.modifyTVar (getJobState m)
                                     (job:)
                                 putStrLn . pack $  "dequeued: " ++ (show jt)
                                 runReaderT (runJob m jt) m
                                 STM.atomically
                                     $ STM.modifyTVar (getJobState m)
                                     (L.delete job)
                             Nothing -> putStrLn "[dequeue] unknown JobType"
                    return ()
        return ()

    getJobState :: master -> JobState

    enqueue :: master -> JobType master -> IO ()
    enqueue m jt = do
        conn <- liftIO $ R.connect R.defaultConnectInfo
        R.runRedis conn $ do
            R.rpush (queueKey m) [BSC.pack $ show jt]
        return ()

    listQueue :: master -> IO (Either String [JobType master])
    listQueue m = do
        conn <- R.connect R.defaultConnectInfo
        exs <- R.runRedis conn $ do
            R.lrange (queueKey m) 0 (-1)
        case exs of
         Right xs ->
             return $ Right $ catMaybes $ map ((readJobType m) . BSC.unpack) xs
         Left r -> return $ Left $ show r

type JobState = TVar [RunningJob]
data RunningJob = RunningJob U.UUID deriving (Eq)
$(deriveToJSON defaultOptions ''RunningJob)
newJobState :: IO (TVar [RunningJob])
newJobState = STM.newTVarIO []
instance ToJSON U.UUID where
    toJSON = String . pack . U.toString

-- | Handler for job manager api routes
type JobHandler master a =
    YesodJobQueue master c => HandlerT JobQueue (HandlerT master IO) a

-- | get job definitions
getJobR :: JobHandler master Value
getJobR = lift $ do
    y <- getYesod
    let f x = object ["type" .= show x, "description" .= describe x]
    let ts = map f $ allJobTypes y
    returnJson $ object ["jobTypes" .= ts]
    

-- | get a list of jobs in queue
getJobQueueR :: JobHandler master Value
getJobQueueR = lift $ do
    y <- getYesod
    Right q <- liftIO $ listQueue y
    returnJson $ object ["queue" .= map show q]
         
-- | enqueue new job
postJobQueueR :: JobHandler master Value
postJobQueueR = lift $ do
    y <- getYesod
    body <- requireJsonBody :: HandlerT master IO PostJobQueueRequest
    case readJobType y (body ^. job) of
     Just jt -> do
         liftIO $ enqueue y jt
         returnJson $ object []
     Nothing -> invalidArgs ["job"]

-- | get a list of running jobs
getJobStateR :: JobHandler master Value
getJobStateR = lift $ do
    y <- getYesod
    s <- liftIO $ STM.readTVarIO (getJobState y)
    returnJson $ object ["running" .= s]    

-- | Job manager UI (get static page. ajax application)
getJobManagerR :: JobHandler master Value
getJobManagerR = lift $ do
    sendResponse ("text/html" :: ByteString, content)
      where content = toContent $(embedFile "app/dist/index.html")
    
-- | JobQueue manager subsite
instance YesodJobQueue master c => YesodSubDispatch JobQueue (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesJobQueue)

getJobQueue :: a -> JobQueue
getJobQueue = const JobQueue

    
