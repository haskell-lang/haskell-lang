{-# LANGUAGE BangPatterns #-}

-- | Get markdown templates.

module HL.Controller.Markdown where

import HL.Controller
import HL.Model.Markdown
import HL.View
import HL.View.Markdown

-- | Render a simple markdown page.
markdownPage :: [Route App] -> Text -> FilePath -> C (Html ())
markdownPage crumbs t name =
  do content <- io (getMarkdown name)
     lucid (markdownV crumbs t content)
