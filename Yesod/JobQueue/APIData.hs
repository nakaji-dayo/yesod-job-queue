module Yesod.JobQueue.APIData where

import ClassyPrelude.Yesod
import Data.Aeson.APIFieldJsonTH
import Control.Lens

data PostJobQueueRequest = PostJobQueueRequest {
    _postJobQueueRequestJob :: String
}

makeFields ''PostJobQueueRequest
deriveApiFieldJSON ''PostJobQueueRequest



data JobQueueClassInfo = JobQueueClassInfo {
    _jobQueueClassInfoClassName :: Text
    , _jobQueueClassInfoValues :: [Text]
    }

makeFields ''JobQueueClassInfo
deriveApiFieldJSON ''JobQueueClassInfo
