{-# LANGUAGE BangPatterns #-}

-- | Get markdown templates.

module HL.Controller.Markdown where

import HL.Controller
import HL.Model.Markdown
import HL.View
import HL.View.Markdown

-- | Render a simple markdown page.
markdownPage :: Text -> FilePath -> C (Html ())
markdownPage t name =
  do content <- io (getMarkdown name)
     lucid (markdownV t content)
