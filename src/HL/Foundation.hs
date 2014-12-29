{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Yesod foundation.

module HL.Foundation
  (module HL.Static
  ,App(..)
  ,Route(..)
  ,Handler
  ,Widget
  ,resourcesApp
  ,Slug(..)
  ,Human(..))
  where

import Data.Monoid
import Data.Text (pack)
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

instance Human (Route App) where
  toHuman r =
    case r of
      CommunityR       -> "Community"
      IrcR             -> "IRC"
      DocumentationR   -> "Documentation"
      HomeR            -> "Home"
      ReloadR          -> "Reload"
      DonateR          -> "Donate"
      MailingListsR    -> "Mailing Lists"
      NewsR            -> "News"
      StaticR{}        -> "Static"
      DownloadsR       -> "Downloads"
      DownloadsForR os -> "Downloads for " <> toHuman os
      WikiR t          -> "Wiki: " <> t
      ReportR i _      -> "Report " <> pack (show i)
      ReportHomeR i    -> "Report " <> pack (show i)
      WikiHomeR{}      -> "Wiki"

instance Slug (Route App) where
  toSlug r =
    case r of
      CommunityR      -> "community"
      IrcR            -> "irc"
      DocumentationR  -> "documentation"
      HomeR           -> "home"
      ReloadR         -> "reload"
      DonateR         -> "donate"
      MailingListsR   -> "mailing-lists"
      NewsR           -> "news"
      StaticR{}       -> "static"
      DownloadsR      -> "downloads"
      WikiR{}         -> "wiki"
      ReportR{}       -> "report"
      ReportHomeR{}   -> "report"
      WikiHomeR{}     -> "wiki"
      DownloadsForR{} -> "downloads"
