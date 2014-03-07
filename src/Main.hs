-- | Web server.

module Main where

import HL.Foundation
import HL.Dispatch ()

import Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do s <- static "static"
     warp 1990 (App s)
