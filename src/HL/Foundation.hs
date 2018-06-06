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
  ,HL.Foundation.Handler
  ,Widget
  ,resourcesApp
  ,Slug(..)
  ,Human(..)
  ,Mode(..))
  where

import HL.Static
import HL.Types

import UnliftIO
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import Network.Wai.Logger
import System.Log.FastLogger
import Yesod
import Yesod.Caching
import Yesod.Core.Types
import Yesod.GitRev (GitRev)
import Yesod.Slug
import Yesod.Static

-- | Generate boilerplate.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Don't log anything to stdout.
instance Yesod App where
  approot = guessApproot
  makeLogger _ =
    do set <- newFileLoggerSet 1000 "/dev/null"
       (date,_) <- clockDateCacher
       return (Logger {loggerSet = set
                      ,loggerDate = date})

  defaultLayout widget =
    do app <- getYesod
       appDefaultLayout app widget

  -- We make no usage of Yesod's session features in this site, so disable it
  -- to avoid unnecessary overhead and cookie header generation.
  makeSessionBackend _ = return Nothing

instance MonadCaching (HandlerFor App) where
  withCacheDir cont =
    do
      dirVar <- fmap appCacheDir getYesod
      withMVar dirVar cont

instance Human (Route App) where
  toHuman r =
    case r of
      CommunityR           -> "Community"
      IrcR                 -> "IRC"
      DocumentationR       -> "Documentation"
      HomeR                -> "Home"
      MailingListsR        -> "Mailing Lists"
      SuccessStoriesR      -> "Success Stories"
      NewsR                -> "News"
      StaticR{}            -> "Static"
      GetStartedR          -> "Get Started"
      GetStartedOSR os     -> "Get Started (" <> toHuman os <> ")"
      AnnouncementsR       -> "Announcements"
      AnnouncementR x      -> "Announcements " <> x
      OldPackagesR{}       -> "Packages"
      OldPackageR p        -> toHuman p
      LibrariesR{}         -> "Libraries"
      LibraryR p           -> toHuman p
      FeedR{}              -> "News Feed"
      GitRevR{}            -> "Build Version"
      InteroR{}            -> "Intero"
      OldTutorialsR{}      -> "Tutorials"
      TutorialR x          -> "Tutorial: " <> x
      LibrariesSingularR   -> "Libraries"
      FaviconR             -> "favicon"
      RobotsR              -> "robots"
      SitemapR             -> "XML sitemap"

instance Slug (Route App) where
  toSlug r =
    case r of
      CommunityR        -> "community"
      IrcR              -> "irc"
      DocumentationR    -> "documentation"
      HomeR             -> "home"
      MailingListsR     -> "mailing-lists"
      SuccessStoriesR   -> "success-stories"
      NewsR             -> "news"
      StaticR{}         -> "static"
      GetStartedR       -> "get-started"
      GetStartedOSR os  -> "get-started-" <> toSlug os
      AnnouncementsR    -> "announcements"
      AnnouncementR x   -> "announcement-" <> x
      OldPackagesR{}    -> "packages"
      OldPackageR x     -> "packages-" <> toSlug x
      LibrariesR{}      -> "libraries"
      LibraryR x        -> "libraries-" <> toSlug x
      FeedR{}           -> "feed"
      GitRevR{}         -> "build-version"
      InteroR{}         -> "intero"
      OldTutorialsR{}   -> "tutorial"
      TutorialR x       -> "tutorial-" <> x
      LibrariesSingularR-> "libraries"
      FaviconR          -> "favicon"
      RobotsR           -> "robots"
      SitemapR          -> "XML sitemap"

instance YesodBreadcrumbs App where
    breadcrumb r =
      case r of
        CommunityR           -> return ("Community",Nothing)
        IrcR                 -> return ("IRC",Nothing)
        DocumentationR       -> return ("Documentation",Nothing)
        HomeR                -> return ("Home",Nothing)
        MailingListsR        -> return ("Mailing Lists",Nothing)
        SuccessStoriesR      -> return ("Success Stories",Nothing)
        NewsR                -> return ("News",Nothing)
        StaticR{}            -> return ("Static",Nothing)
        GetStartedR          -> return ("Get Started",Nothing)
        GetStartedOSR os     -> return ("Get Started (" <> toHuman os <> ")",Nothing)
        AnnouncementsR       -> return ("Announcements",Nothing)
        AnnouncementR x      -> return ("Announcement " <> x,Just AnnouncementsR)
        OldPackagesR{}       -> return ("Packages",Nothing)
        OldPackageR p        -> return (toHuman p,Nothing)
        LibrariesR{}         -> return ("Libraries",Just DocumentationR)
        LibraryR p           -> do
            tutorials <- fmap appTutorials getYesod
            let title = maybe (toHuman p) tutorialTitle
                        (Map.lookup (PackageTutorial p) tutorials)
            return ("Tutorial: " <> title,Just LibrariesR)
        FeedR{}              -> return ("News Feed",Nothing)
        GitRevR{}            -> return ("Build Version",Nothing)
        InteroR{}            -> return ("Intero",Nothing)
        OldTutorialsR{}      -> return ("Tutorials",Just DocumentationR)
        TutorialR x          -> do
            tutorials <- fmap appTutorials getYesod
            let title = maybe x tutorialTitle (Map.lookup (RegularTutorial x) tutorials)
            return ("Tutorial: " <> title,Just DocumentationR)
        LibrariesSingularR   -> return ("Libraries",Just DocumentationR)
        FaviconR             -> return ("favicon", Nothing)
        RobotsR              -> return ("robots", Nothing)
        SitemapR             -> return ("XML sitemap", Nothing)
