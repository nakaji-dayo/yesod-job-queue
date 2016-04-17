module Yesod.JobQueue.Scheduler where

import Yesod.JobQueue
import System.Cron.Schedule

import qualified Prelude as P
import ClassyPrelude.Yesod

class (YesodJobQueue master config) => YesodJobQueueScheduler master config where
    -- | job schedules
    getJobSchedules :: master -> [(String, JobType master)]

    -- | start schedule
    startJobSchedule :: (MonadBaseControl IO m, MonadIO m) => master -> m ()
    startJobSchedule master = do
        let add (s, jt) = addJob (enqueue master jt) s
        tids <- liftIO $ execSchedule $ mapM_ add $ getJobSchedules master
        print tids
