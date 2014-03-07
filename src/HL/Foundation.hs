{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Yesod foundation.

module HL.Foundation
  (module HL.Static
  ,module Yesod.Blaze
  ,App(..)
  ,Route(..)
  ,Handler
  ,Widget
  ,resourcesApp)
  where

import HL.Static

import Control.Concurrent.Chan
import Network.Wai.Logger
import System.Log.FastLogger
import Yesod
import Yesod.Blaze
import Yesod.Core.Types
import Yesod.Static

-- | Application state.
data App = App
  { appStatic :: Static
  , appReload :: Chan ()
  }

-- | Generate boilerplate.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Don't log anything to stdout.
instance Yesod App where
  makeLogger _ = do set <- newFileLoggerSet 1000 "/dev/null"
                    (date,_) <- clockDateCacher
                    return (Logger {loggerSet = set
                                   ,loggerDate = date})
