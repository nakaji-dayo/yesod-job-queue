{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.JobQueue.Routes where

import Data.Text
import Yesod

-- Subsites have foundations just like master sites.
data JobQueue = JobQueue

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "JobQueue" [parseRoutes|
/ JobR GET
/queue JobQueueR GET POST
/state JobStateR GET
/manager JobManagerR GET
/manager/#Text JobManagerStaticR GET
|]
