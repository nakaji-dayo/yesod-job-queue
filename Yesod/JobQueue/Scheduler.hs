-- | Cron Job for Yesod
module Yesod.JobQueue.Scheduler where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, Text)
import System.Cron.Schedule
import Yesod.JobQueue
import Yesod.JobQueue.Types


-- | Cron Scheduler for YesodJobQueue
class (YesodJobQueue master) => YesodJobQueueScheduler master where
    -- | job schedules
    getJobSchedules :: master -> [(T.Text, JobType master)]

    -- | start schedule
    startJobSchedule :: MonadUnliftIO m => master -> m ()
    startJobSchedule master = do
        let add (s, jt) = addJob (enqueue master jt) s
        tids <- liftIO $ execSchedule $ mapM_ add $ getJobSchedules master
        liftIO $ print tids

-- | Need by 'getClassInformation'
schedulerInfo :: YesodJobQueueScheduler master => master ->  JobQueueClassInfo
schedulerInfo m = JobQueueClassInfo "Scheduler" $  map showSchedule $ getJobSchedules m
  where showSchedule (s, jt) = s <> " | " <> (T.pack $ show jt)
