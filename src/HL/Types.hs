{-# LANGUAGE DeriveDataTypeable #-}

-- | Side-wide datatypes.

module HL.Types where

import Control.Concurrent.Chan
import Control.Exception
import Data.Typeable
import Yesod.Static

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
