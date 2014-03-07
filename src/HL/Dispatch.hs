{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.Controller.Home
import HL.Foundation

mkYesodDispatch "App" resourcesApp
