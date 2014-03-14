-- | Web server.

module Main where

import           HL.Foundation
import           HL.Dispatch ()
import           HL.C.Theme

import           Control.Concurrent.Chan
import qualified Data.Text.Lazy.IO as L
import           System.Directory
import           Yesod.Static

-- | Main entry point.
main :: IO ()
main =
  do s <- static "static"
     c <- newChan
     setupCache
     warp 2001 (App s c)

setupCache =
  do createDirectoryIfMissing True "cache/"
     L.writeFile "cache/theme.css" (themeCss)
