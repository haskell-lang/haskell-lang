{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Make the dispatcher.

module HL.Dispatch where

import HL.Foundation
import HL.C.Home

mkYesodDispatch "App" resourcesApp
