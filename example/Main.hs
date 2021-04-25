{-# LANGUAGE DerivingStrategies, StandaloneDeriving, UndecidableInstances, DataKinds, FlexibleInstances #-}

import qualified Prelude as P ()
import ClassyPrelude.Yesod
import Yesod.JobQueue
import Yesod.JobQueue.Scheduler
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Control.Concurrent (threadDelay)


-- Yesod Persist settings (Nothing special here)
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  age Int
  deriving Show
|]
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        app <- getYesod
        runSqlPool action $ appConnPool app


-- Make Yesod App that have ConnectionPool, JobState
data App = App {
    appConnPool :: ConnectionPool
    , appDBConf :: SqliteConf
    , appJobState :: JobState
    }
instance Yesod App
mkYesod "App" [parseRoutes|
/ HomeR GET
/job JobQueueR JobQueue getJobQueue -- ^ JobQueue API and Manager
|]

-- JobQueue settings
data MyJobType = AggregationUser
               | PushNotification
               | HelloJob String
               deriving (Show, Read, Generic)

instance YesodJobQueue App where
    type JobType App = MyJobType
    getJobState = appJobState
    threadNumber _ = 2
    runDBJob action = do
        app <- ask
        runSqlPool action $ appConnPool app
    runJob _ AggregationUser = do
        us <- runDBJob $ selectList ([] :: [Filter Person]) []
        liftIO $ threadDelay $ 10 * 1000 * 1000
        print us
        putStrLn "complate job!"
    runJob _ PushNotification = do
        putStrLn "sent notification!"
    runJob _ (HelloJob name) = do
        putStrLn . pack $ "Hello " ++ name
    getClassInformation app = [jobQueueInfo app, schedulerInfo app]
    describeJob _ "AggregationUser" = Just "aggregate user's activities"
    describeJob _ _ = Nothing
    -- jobManagerJSUrl _ = "http://localhost:3001/dist/app.bundle.js" -- use for development with "npm run bs"
    -- queueConnectInfo _ = R.defaultConnectInfo
    --                      {R.connectHost = "127.0.0.1"
    --                      , R.connectPort = R.PortNumber 6379}

instance YesodJobQueueScheduler App  where
    getJobSchedules _ = [("* * * * *", AggregationUser)
                         , ("* * * * *", HelloJob "Foo")]

-- Handlers
getHomeR :: HandlerT App IO Html
getHomeR = defaultLayout $ do
    setTitle "JobQueue sample"
    [whamlet|
        <h1>Hello
    |]

-- Main
main :: IO ()
main = runStderrLoggingT $
       withSqlitePool (sqlDatabase dbConf) (sqlPoolSize dbConf) $ \pool -> liftIO $ do
           runResourceT $ flip runSqlPool pool $ do
               runMigration migrateAll
               insert $ Person "test" 28
           jobState <- newJobState -- ^ create JobState
           let app = App pool dbConf jobState
           startDequeue app
           startJobSchedule app
           warp 3000 app
  where dbConf = SqliteConf "test.db3" 4
