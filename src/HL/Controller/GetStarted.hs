{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.GetStarted where

import HL.Controller
import HL.Model.Markdown
import HL.View.GetStarted
import HL.View

-- | GetStarted controller.
getGetStartedR :: C (Html ())
getGetStartedR = lucid . downloadsFromMarkdown =<< io (getMarkdown "get-started.md")
