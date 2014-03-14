-- | Reload poller.

module HL.C.Reload where

import HL.C

import Control.Concurrent.Chan.Lifted

-- | Reload controller.
getReloadR :: C ()
getReloadR =
  do reload <- fmap appReload getYesod
     dupChan reload >>= readChan
