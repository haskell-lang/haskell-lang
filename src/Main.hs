-- | Web server.

module Main where

import Control.Concurrent.MVar
import HL.Dispatch ()
import HL.Foundation
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath
import Yesod
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
     putStrLn ("Now running at: http://localhost:" ++ show port ++ "/")
     warp port (App st cacheVar)
