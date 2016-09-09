{-# LANGUAGE TemplateHaskell #-}

module Yesod.JobQueue.Types where

import Data.Aeson.APIFieldJsonTH
import Data.Text (Text)
import Control.Lens (makeFields)

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
