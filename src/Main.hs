-- | Web server.

module Main where

import HL.Dispatch ()
import HL.Foundation



import Control.Concurrent.Chan
import System.Environment (getEnvironment)
import Yesod
import Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do dir <- getStaticDir
     s <- static dir
     c <- newChan
     env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     warp port (App s c)
