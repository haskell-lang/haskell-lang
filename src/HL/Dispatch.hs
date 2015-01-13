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
import HL.Controller.Downloads
import HL.Controller.Home
import HL.Controller.News
import HL.Controller.Reload
import HL.Controller.Report
import HL.Controller.Wiki
import HL.Controller.Irc
import HL.Controller.MailingLists
import HL.Foundation

import Control.Monad.Identity
import Lucid
import Yesod hiding (Html)

instance ToTypedContent (Html ()) where
  toTypedContent html =
    TypedContent "text/html"
                 (ContentBuilder (runIdentity (execHtmlT html)) Nothing)

instance ToContent (Html ()) where
  toContent = undefined

mkYesodDispatch "App" resourcesApp
