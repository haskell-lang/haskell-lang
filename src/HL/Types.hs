{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Side-wide datatypes.

module HL.Types where

import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           HL.Model.Packages
import           Yesod.Core (HandlerT, WidgetT, Html)
import           Yesod.Core.Dispatch
import           Yesod.Feed
import           Yesod.GitRev (GitRev)
import           Yesod.Slug
import           Yesod.Static

-- | Make a human-readable version of the value.
class Human a where
  toHuman :: a -> Text

-- | A haskell-lang exception.
data HaskellLangException
  = MarkdownFileUnavailable !FilePath
  | ReportPageNotFound !FilePath
  deriving (Show,Typeable,Eq)

instance Exception HaskellLangException

-- | Application state.
data App = App
  { appStatic        :: !Static
  , appCacheDir      :: !(MVar FilePath)
  , appPackageInfo   :: !PackageInfo
  , appDefaultLayout :: !(WidgetT App IO () -> HandlerT App IO Html)
  , appFeedEntries   :: ![FeedEntry Text]
  , appGitRev        :: !GitRev
  }

-- | Operating system. Used for downloads, for example.
data OS = Windows | OSX | Linux
  deriving (Read,Show,Typeable,Eq,Enum,Bounded)

instance Slug OS where
  toSlug o =
    case o of
      Windows -> "windows"
      OSX     -> "osx"
      Linux   -> "linux"

instance Human OS where
  toHuman o =
    case o of
      Windows -> "Windows"
      OSX     -> "OS X"
      Linux   -> "Linux"

instance PathPiece OS where
  toPathPiece = toSlug
  fromPathPiece t =
    case t of
      "osx"     -> Just OSX
      "windows" -> Just Windows
      "linux"   -> Just Linux
      _         -> Nothing

-- | Mode for rendering Haskell report.
data Mode = Mono | Node
  deriving (Eq,Show,Read)

instance Slug Mode where
  toSlug m =
    case m of
      Mono -> "mono"
      Node -> "node"

instance PathPiece Mode where
  fromPathPiece m =
    case m of
      "mono" -> Just Mono
      "node" -> Just Node
      _ -> Nothing
  toPathPiece = toSlug

-- | Page cache key.
data PageKey = Report !Int

instance Slug PageKey where
  toSlug (Report year) = "report-" <> T.pack (show year)
