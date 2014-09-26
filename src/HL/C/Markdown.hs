{-# LANGUAGE BangPatterns #-}

-- | Get markdown templates.

module HL.C.Markdown where

import           HL.C
import           HL.M.Markdown
import           HL.V.Markdown

-- | Render a simple markdown page.
markdownPage :: [Route App] -> Text -> FilePath -> C Html
markdownPage crumbs t name =
  do content <- io (getMarkdown name)
     blaze (markdownV crumbs t content)
