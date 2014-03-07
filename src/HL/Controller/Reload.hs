-- | Reload poller.

module HL.Controller.Reload where

import HL.Foundation
import HL.View.Home

import Control.Concurrent.Chan.Lifted

-- | Reload controller.
getReloadR :: Handler ()
getReloadR =
  do reload <- fmap appReload getYesod
     dupChan reload >>= readChan
