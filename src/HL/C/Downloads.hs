{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.C.Downloads where

import HL.C.Markdown
import HL.C

-- | Downloads controller.
getDownloadsR :: C Html
getDownloadsR =
  markdownPage [DownloadsR] "Downloads" "downloads.md"
