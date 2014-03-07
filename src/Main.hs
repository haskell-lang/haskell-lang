-- | Web server.

module Main where

import HL.Foundation
import HL.Dispatch ()

import Control.Concurrent.Chan
import Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do s <- static "static"
     c <- newChan
     warp 1990 (App s c)
