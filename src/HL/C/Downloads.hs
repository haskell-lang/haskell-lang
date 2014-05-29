{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.C.Downloads where

import HL.C
import HL.V.Downloads

-- | Downloads controller.
getDownloadsR :: C Html
getDownloadsR = senza downloadsV

-- | Downloads for particular OS.
getDownloadsForR :: OS -> C Html
getDownloadsForR = senza . downloadsForV
