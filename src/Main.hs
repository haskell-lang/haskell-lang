-- | Web server.

module Main where

import HL.Dispatch ()
import HL.Foundation

import Control.Concurrent.Chan
import Yesod
import Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do s <- static "static"
     c <- newChan
     warp 1990 (App s c)
