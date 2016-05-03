{-# LANGUAGE ImpredicativeTypes, UndecidableInstances #-}
-- |
--
-- Background jobs library for Yesod.
-- Use example is in README.md.
--
module Yesod.JobQueue (
    YesodJobQueue (..)
    , JobQueue
    , JobInfo (..)
    , startDequeue
    , enqueue
    , JobState
    , newJobState
    , jobQueueInfo
    , getJobQueue
    ) where

import qualified Prelude as P

import Yesod.JobQueue.Routes
import Yesod.JobQueue.Types

import qualified Data.List as L
import ClassyPrelude.Yesod
import Control.Concurrent
import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.Text as T (pack, append)
import Control.Monad.Logger
import qualified Database.Redis as R

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import qualified Control.Concurrent.STM as STM
import Data.Time.Clock

import Data.Aeson.TH

import Data.FileEmbed (embedFile)


-- | description for JobType
class JobInfo j where
    describe :: j -> String
    describe _ = ""

-- | Thread ID for convenience
type ThreadNum = Int

-- | JobType String
type JobTypeString = String

-- | Information of the running job
data RunningJob = RunningJob {
    jobType :: JobTypeString
    , threadId :: ThreadNum
    , jobId :: U.UUID
    , startTime :: UTCTime
    } deriving (Eq)
instance ToJSON U.UUID where
    toJSON = String . pack . U.toString
$(deriveToJSON defaultOptions ''RunningJob)

-- | Manage the running jobs
type JobState = TVar [RunningJob]

-- | create new JobState
newJobState :: IO (TVar [RunningJob])
newJobState = STM.newTVarIO []

data JobQueueItem = JobQueueItem {
    queueJobType :: JobTypeString
    , queueTime :: UTCTime
} deriving (Show, Read)
$(deriveToJSON defaultOptions ''JobQueueItem)

-- | Backend jobs for Yesod
class (Yesod master, Read (JobType master), Show (JobType master)
      , Enum (JobType master), Bounded (JobType master)
      , JobInfo (JobType master))
      => YesodJobQueue master where
    
    -- | Custom Job Type
    type JobType master
    
    -- | Job Handler
    runJob :: (MonadBaseControl IO m, MonadIO m)
              => master -> JobType master -> ReaderT master m ()

    -- | connection info for redis
    queueConnectInfo :: master -> R.ConnectInfo
    queueConnectInfo _ = R.defaultConnectInfo
    
    -- | queue key name for redis
    queueKey :: master -> ByteString
    queueKey _ = "yesod-job-queue"

    -- | The number of threads to run the job
    threadNumber :: master -> Int
    threadNumber _ = 1
        
    -- | runDB for job
    runDBJob :: (MonadBaseControl IO m, MonadIO m)
                => ReaderT (YesodPersistBackend master) (ReaderT master m) a
                -> ReaderT master m a

    -- | get job state
    getJobState :: master -> JobState

    -- | get API and Manager base url
    jobAPIBaseUrl :: master -> String
    jobAPIBaseUrl _  = "/job"

    -- | get manager application javascript url (change only development)
    jobManagerJSUrl :: master -> String
    jobManagerJSUrl m = (jobAPIBaseUrl m) ++ "/manager/app.js"

    -- | get information of all type classes related job-queue
    getClassInformation :: master -> [JobQueueClassInfo]
    getClassInformation m = [jobQueueInfo m]

startDequeue :: (YesodJobQueue master, MonadBaseControl IO m, MonadIO m) => master -> m ()
startDequeue m = do
    let num = threadNumber m
    forM_ [1 .. num] $ startThread m

-- | start dequeue-ing job in new thread
startThread :: (YesodJobQueue master, MonadBaseControl IO m, MonadIO m) => master -> ThreadNum -> m ()
startThread m tNo = do
    liftIO $ forkIO $ do
        conn <- R.connect $ queueConnectInfo m
        R.runRedis conn $ do
            forever $ do
                emr <- R.blpop [queueKey m] 600
                liftIO $ case emr of
                    Left e -> putStrLn "[dequeue] error in at connection redis"
                    Right Nothing -> putStrLn "[dequeue] timeout retry"
                    Right (Just (k, v)) -> do
                        let item = readMay $ BSC.unpack v :: Maybe JobQueueItem
                        case readJobType m =<< queueJobType <$> item of
                         Just jt -> do
                             jid <- U.nextRandom
                             time <- getCurrentTime
                             let job = RunningJob (show jt) tNo jid time
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

-- | Add job to end of the queue
enqueue :: YesodJobQueue master => master -> JobType master -> IO ()
enqueue m jt = do
    time <- getCurrentTime
    let item = JobQueueItem (show jt) time
    conn <- liftIO $ R.connect $ queueConnectInfo m
    R.runRedis conn $ do
        R.rpush (queueKey m) [BSC.pack $ show item]
    return ()

-- | Get all job in the queue
listQueue :: YesodJobQueue master => master -> IO (Either String [JobQueueItem])
listQueue m = do
    conn <- R.connect $ queueConnectInfo m
    exs <- R.runRedis conn $ do
        R.lrange (queueKey m) 0 (-1)
    case exs of
     Right xs ->
         return $ Right $ catMaybes
         $ map (readMay . BSC.unpack) xs
     Left r -> return $ Left $ show r

-- | read JobType from String
readJobType :: YesodJobQueue master => master -> String -> Maybe (JobType master)
readJobType _ = readMay

-- | get all job type list
allJobTypes :: (YesodJobQueue master) => master -> [JobType master]
allJobTypes _ = [minBound..]

-- | Need by 'getClassInformation'
jobQueueInfo :: YesodJobQueue master => master ->  JobQueueClassInfo
jobQueueInfo m = JobQueueClassInfo "JobQueue" [threadInfo]
  where threadInfo = "Number of threads: " `T.append` (T.pack . show $ threadNumber m)


-- | Handler for job manager api routes
type JobHandler master a =
    YesodJobQueue master => HandlerT JobQueue (HandlerT master IO) a

-- | get job definitions
getJobR :: JobHandler master Value
getJobR = lift $ do
    y <- getYesod
    -- job types
    let f x = object ["type" .= show x, "description" .= describe x]
    let ts = map f $ allJobTypes y
    -- type class info
    let info = getClassInformation y
    returnJson $ object ["jobTypes" .= ts, "information" .= info]

-- | get a list of jobs in queue
getJobQueueR :: JobHandler master Value
getJobQueueR = lift $ do
    y <- getYesod
    Right q <- liftIO $ listQueue y
    returnJson $ object ["queue" .= q]
         
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

getJobManagerR :: JobHandler master Html
getJobManagerR = lift $ do
    y <- getYesod
    withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>YesodJobQueue Manager
        <link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
        <link rel="stylesheet" href="https://code.getmdl.io/1.1.3/material.blue_grey-red.min.css">
        <script defer src="https://code.getmdl.io/1.1.3/material.min.js">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <body>
        <div class="mdl-layout mdl-js-layout mdl-layout--fixed-header">
            <header class="mdl-layout__header">
                <div class="mdl-layout__header-row">
                    <!-- Title -->
                    <span class="mdl-layout-title">YesodJobQueue Manager
            <main class="mdl-layout__content">
                <div id="app" class="page-content">
        <div id="demo-toast-example" class="mdl-js-snackbar mdl-snackbar">
            <div class="mdl-snackbar__text">
            <button class="mdl-snackbar__action" type="button">
        <script>
            window.BASE_URL = "#{jobAPIBaseUrl y}"
        <script src="#{jobManagerJSUrl y}">
|]

-- | Job manager UI (get static page. ajax application)
getJobManagerStaticR :: Text -> JobHandler master Value
getJobManagerStaticR f
    | f == "app.js" = lift $ do
          let content = toContent $(embedFile "app/dist/app.bundle.js")
          sendResponse ("application/json" :: ByteString, content)
    | otherwise = notFound

-- | JobQueue manager subsite
instance YesodJobQueue master => YesodSubDispatch JobQueue (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesJobQueue)

getJobQueue :: a -> JobQueue
getJobQueue = const JobQueue

    
