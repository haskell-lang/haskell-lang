{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Web server.

module HL.Server where

import HL.Foundation
import HL.Dispatch ()

import Yesod.Static

-- | Start the web server.
startServer :: IO ()
startServer =
  do s <- static "static"
     warp 1990 (App s)
