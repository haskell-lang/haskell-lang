{-# LANGUAGE TemplateHaskell #-}
-- | Web server.

module HL.Main
    ( prodMain
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

-- | Main entry point.
prodMain :: IO ()
prodMain =
  do dir <- getStaticDir
     st <- static dir
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     snippets <- getSnippets
     packageInfo <- getPackageInfo
     entries <- getFeedEntries
     tutorials <- getTutorials False
     putStrLn ("Now running at: http://localhost:" ++ show port ++ "/")
     warp port App
         { appStatic = st
         , appCacheDir = cacheVar
         , appPackageInfo = packageInfo
         , appDefaultLayout = defaultLayoutImpl
         , appFeedEntries = map toFeedEntry entries
         , appGitRev = $gitRev
         , appSnippetInfo = snippets
         , appTutorials = tutorials
         }

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
