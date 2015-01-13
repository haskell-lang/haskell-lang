{-# LANGUAGE OverloadedStrings #-}

-- | Simple markdown page view.

module HL.View.Markdown where

import HL.View
import HL.View.Template

-- | Render a simple page.
markdownV :: [Route App] -> Text -> Html () -> FromLucid App
markdownV routes t inner =
  template routes t (const (container_ (row_ (span12_ [class_ "col-md-12"] inner))))
