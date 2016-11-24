{-# LANGUAGE TemplateHaskell #-}
-- | Web server.

module HL.Main
    ( mkApp
    , prodMain
    , develMain
    ) where

import           Control.Concurrent.Async (race_)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           HL.Controller.Feed (toFeedEntry, getFeedEntries)
import           HL.Dispatch ()
import           HL.Foundation
import           HL.Model.Packages
import           HL.Model.Snippets
import           HL.Model.Tutorial
import           HL.View.Template
import           System.Directory
import           System.Environment (getEnvironment)
import           System.FilePath
import           Yesod
import           Yesod.GitRev (gitRev)
import           Yesod.Static

mkApp :: IO App
mkApp =
  do dir <- getStaticDir
     st <- static dir
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     snippets <- getSnippets
     packageInfo <- getPackageInfo
     entries <- getFeedEntries
     tutorials <- getTutorials False
     return App
         { appStatic = st
         , appCacheDir = cacheVar
         , appPackageInfo = packageInfo
         , appDefaultLayout = defaultLayoutImpl
         , appFeedEntries = map toFeedEntry entries
         , appGitRev = $gitRev
         , appSnippetInfo = snippets
         , appTutorials = tutorials
         }

-- | Main entry point.
prodMain :: IO ()
prodMain =
  do env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     app <- mkApp
     putStrLn ("Now running at: http://localhost:" ++ show port ++ "/")
     warp port app

develMain :: IO ()
develMain = race_ watchTermFile prodMain

watchTermFile :: IO ()
watchTermFile =
    loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop
