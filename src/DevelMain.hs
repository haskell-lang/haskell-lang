{-# LANGUAGE TemplateHaskell #-}
-- | Devel web server.

module DevelMain where

import           Control.Concurrent
import           Control.Exception
import           Data.IORef
import qualified Data.Yaml as Yaml
import           Foreign.Store
import           HL.Controller.Feed (toFeedEntry)
import           HL.Dispatch ()
import           HL.Foundation
import           HL.Model.Packages
import           HL.Model.Snippets
import           HL.Model.Tutorial
import           HL.View.Template
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.Environment (getEnvironment)
import           System.FilePath
import           Yesod
import           Yesod.GitRev (gitRev)
import           Yesod.Static

-- | Start the web server.
main :: IO (Store (IORef Application))
main =
  do st <- static "static"
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     packageInfo <- getPackageInfo False
     snippets <- getSnippets
     entries <- Yaml.decodeFileEither "config/feed-entries.yaml"
            >>= either throwIO return
     tutorials <- getTutorials
     app <- toWaiApp (App
       { appStatic = st
       , appCacheDir = cacheVar
       , appPackageInfo = packageInfo
       , appDefaultLayout = defaultLayoutImpl
       , appFeedEntries = map toFeedEntry entries
       , appGitRev = $gitRev
       , appSnippetInfo = snippets
       , appTutorials = tutorials
       })
     ref <- newIORef app
     env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     tid <- forkIO
              (runSettings
                 (setPort port defaultSettings)
                 (\req send ->
                    do handler <- readIORef ref
                       handler req send))
     _ <- newStore tid
     ref' <- newStore ref
     _ <- newStore cacheVar
     return ref'

-- | Update the server, start it if not running.
update :: IO (Store (IORef Application))
update =
  do m <- lookupStore 1
     case m of
       Nothing -> main
       Just store ->
         do ref <- readStore store
            cacheVar <- readStore (Store 2)
            st <- static "static"
            packageInfo <- getPackageInfo False
            snippets <- getSnippets
            print snippets
            entries <- Yaml.decodeFileEither "config/feed-entries.yaml"
                   >>= either throwIO return
            tutorials <- getTutorials
            app <- toWaiApp (App st cacheVar packageInfo defaultLayoutImpl (map toFeedEntry entries) $gitRev snippets tutorials)
            writeIORef ref app
            return store
