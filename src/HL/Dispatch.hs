{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.C.Community
import HL.C.Documentation
import HL.C.Downloads
import HL.C.Home
import HL.C.News
import HL.C.Reload
import HL.C.Report
import HL.C.Wiki
import HL.C.Irc
import HL.C.MailingLists
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
