{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.Controller.Home
import HL.Controller.Reload
import HL.Controller.Theme
import HL.Controller.Downloads
import HL.Controller.Community
import HL.Controller.Documentation
import HL.Controller.News
import HL.Foundation

mkYesodDispatch "App" resourcesApp
