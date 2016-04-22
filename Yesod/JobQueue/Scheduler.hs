-- | Cron Job for Yesod
module Yesod.JobQueue.Scheduler where

import Yesod.JobQueue
import Yesod.JobQueue.Types
import System.Cron.Schedule

import qualified Prelude as P
import ClassyPrelude.Yesod

import qualified Data.Text as T (pack)

-- | Cron Scheduler for YesodJobQueue
class (YesodJobQueue master) => YesodJobQueueScheduler master where
    -- | job schedules
    getJobSchedules :: master -> [(String, JobType master)]

    -- | start schedule
    startJobSchedule :: (MonadBaseControl IO m, MonadIO m) => master -> m ()
    startJobSchedule master = do
        let add (s, jt) = addJob (enqueue master jt) s
        tids <- liftIO $ execSchedule $ mapM_ add $ getJobSchedules master
        print tids

-- | Need by 'getClassInformation'
schedulerInfo :: YesodJobQueueScheduler master => master ->  JobQueueClassInfo
schedulerInfo m = JobQueueClassInfo "Scheduler" $  map (T.pack . showSchedule) $ getJobSchedules m
  where showSchedule (s, jt) = s ++ " | " ++ (show jt)
