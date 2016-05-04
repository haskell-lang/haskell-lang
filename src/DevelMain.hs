-- | Devel web server.

module DevelMain where

import           HL.Dispatch ()
import           HL.Foundation
import           HL.View.Template

import Control.Exception (throwIO)
import           Control.Concurrent
import           Data.IORef
import qualified Data.Yaml as Yaml
import           Foreign.Store
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.Environment (getEnvironment)
import           System.FilePath
import           Yesod
import           Yesod.Static

-- | Start the web server.
main :: IO (Store (IORef Application))
main =
  do st <- static "static"
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     packageInfo <- Yaml.decodeFileEither "config/package-info.yaml"
                >>= either throwIO return
     app <- toWaiApp (App
       { appStatic = st
       , appCacheDir = cacheVar
       , appPackageInfo = packageInfo
       , appDefaultLayout = defaultLayoutImpl
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
            packageInfo <- Yaml.decodeFileEither "config/package-info.yaml"
                       >>= either throwIO return
            app <- toWaiApp (App st cacheVar packageInfo defaultLayoutImpl)
            writeIORef ref app
            return store
