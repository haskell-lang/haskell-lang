{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Side-wide datatypes.

module HL.Types where

import Control.Concurrent.Chan
import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Yesod
import Yesod.Static

-- | Make a human-readable version of the value.
class Human a where
  toHuman :: a -> Text

-- | Make a slug version of the value.
class Slug a where
  toSlug :: a -> Text

-- | A haskell-lang exception.
data HaskellLangException
  = MarkdownFileUnavailable !FilePath
  | ReportPageNotFound !FilePath
  deriving (Show,Typeable,Eq)

instance Exception HaskellLangException

-- | Application state.
data App = App
  { appStatic :: !Static
  , appReload :: !(Chan ())
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
