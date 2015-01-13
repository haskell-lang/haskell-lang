-- | Reload poller.

module HL.Controller.Reload where

import HL.Controller

import Control.Concurrent.Chan.Lifted

-- | Reload controller.
getReloadR :: C ()
getReloadR =
  do reload <- fmap appReload getYesod
     dupChan reload >>= readChan
