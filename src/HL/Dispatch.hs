{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.Controller.Community
import HL.Controller.Documentation
import HL.Controller.Donate
import HL.Controller.GetStarted
import HL.Controller.Announcements
import HL.Controller.Home
import HL.Controller.Irc
import HL.Controller.MailingLists
import HL.Controller.News
import HL.Controller.Report
import HL.Controller.Wiki
import HL.Controller.Packages
import HL.Controller.Intero
import HL.Controller.Feed
import HL.Controller.Tutorial
import HL.Foundation
import Yesod.Core.Dispatch

mkYesodDispatch "App" resourcesApp
