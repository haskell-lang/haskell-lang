-- | Main entry point to haskell-lang.
--
-- Haskell web site.

module Main where

import HL.Server

-- | Main entry point.
main :: IO ()
main =
 startServer
