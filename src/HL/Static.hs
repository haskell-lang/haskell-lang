{-# LANGUAGE TemplateHaskell #-}

-- | Static files.

module HL.Static where

import Control.Monad.IO.Class
import Yesod.Static

staticFiles "static/"

-- | Get the directory for static files. In development returns the
-- local copy, in production mode uses the Cabal data-files
-- functionality.
getStaticDir :: MonadIO m => m FilePath
getStaticDir =
  return "static/"
