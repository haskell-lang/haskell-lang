{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module HL.C.Home where

import HL.C.Markdown
import HL.C

-- | Home controller.
getHomeR :: C Html
getHomeR =
  markdownPage [] "Home" "home.md"
