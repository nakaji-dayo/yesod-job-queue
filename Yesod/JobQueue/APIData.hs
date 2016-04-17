module Yesod.JobQueue.APIData where

import ClassyPrelude.Yesod
import Data.Aeson.APIFieldJsonTH
import Control.Lens

data PostJobQueueRequest = PostJobQueueRequest {
    _postJobQueueRequestJob :: String
}

makeFields ''PostJobQueueRequest
deriveApiFieldJSON ''PostJobQueueRequest

