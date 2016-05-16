{-# LANGUAGE TemplateHaskell #-}
-- | Web server.

module Main where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import qualified Data.Yaml as Yaml
import HL.Controller.Feed (toFeedEntry)
import HL.Dispatch ()
import HL.Foundation
import HL.View.Template
import HL.Model.Packages
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath
import Yesod
import Yesod.GitRev (gitRev)
import Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do dir <- getStaticDir
     st <- static dir
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     packageInfo <- getPackageInfo
     entries <- Yaml.decodeFileEither "config/feed-entries.yaml"
            >>= either throwIO return
     putStrLn ("Now running at: http://localhost:" ++ show port ++ "/")
     warp port App
         { appStatic = st
         , appCacheDir = cacheVar
         , appPackageInfo = packageInfo
         , appDefaultLayout = defaultLayoutImpl
         , appFeedEntries = map toFeedEntry entries
         , appGitRev = $gitRev
         }
