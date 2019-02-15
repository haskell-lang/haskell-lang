{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.Controller.Deprecated
import HL.Controller.Announcements
import HL.Controller.Intero
import HL.Controller.Feed
import HL.Controller.Standard
import HL.Controller.Redirects
import HL.Foundation
import Yesod.Core.Dispatch

mkYesodDispatch "App" resourcesApp
