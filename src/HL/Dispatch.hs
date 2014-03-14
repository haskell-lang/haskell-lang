{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.C.Community
import HL.C.Documentation
import HL.C.Downloads
import HL.C.Home
import HL.C.News
import HL.C.Reload
import HL.C.Report
import HL.C.Theme
import HL.C.Wiki
import HL.Foundation

import Yesod

mkYesodDispatch "App" resourcesApp
