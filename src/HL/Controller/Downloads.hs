{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.Controller.Downloads where

import HL.Controller
import HL.Model.Markdown
import HL.View.Downloads
import HL.View

-- | Downloads controller.
getDownloadsR :: C (Html ())
getDownloadsR = lucid . downloadsFromMarkdown =<< io (getMarkdown "downloads-main.md")
