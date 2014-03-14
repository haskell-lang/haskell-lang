{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Yesod foundation.

module HL.Foundation
  (module HL.Static
  ,App(..)
  ,Route(..)
  ,Handler
  ,Widget
  ,resourcesApp)
  where

import HL.Static
import HL.Types

import Data.Text (Text)
import Network.Wai.Logger
import System.Log.FastLogger
import Yesod
import Yesod.Core.Types
import Yesod.Static

-- | Generate boilerplate.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Don't log anything to stdout.
instance Yesod App where
  makeLogger _ =
    do set <- newFileLoggerSet 1000 "/dev/null"
       (date,_) <- clockDateCacher
       return (Logger {loggerSet = set
                      ,loggerDate = date})
