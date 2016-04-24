{-# LANGUAGE TemplateHaskell #-}

-- | Static files.

module HL.Static where

import Control.Monad.IO.Class
import HL.Development

import Paths_haskell_lang
import Yesod.Static

staticFiles "static/"

-- | Get the directory for static files. In development returns the
-- local copy, in production mode uses the Cabal data-files
-- functionality.
getStaticDir :: MonadIO m => m FilePath
getStaticDir =
  if development
     then return "static/"
     else liftIO (getDataFileName "static")
