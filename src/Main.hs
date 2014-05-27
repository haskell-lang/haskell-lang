-- | Web server.

module Main where

import           HL.Foundation
import           HL.Dispatch ()

import           Control.Concurrent.Chan
import qualified Data.Text.Lazy.IO as L
import           System.Directory
import           Yesod
import           Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do s <- static "static"
     c <- newChan
     warp 2001 (App s c)
