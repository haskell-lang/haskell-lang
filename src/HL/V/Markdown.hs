{-# LANGUAGE OverloadedStrings #-}

-- | Simple markdown page view.

module HL.V.Markdown where

import HL.V
import HL.V.Template

-- | Render a simple page.
markdownV :: [Route App] -> Text -> Html -> Blaze App
markdownV routes t inner =
  template
    routes
    t
    (const (container (row (span12 inner))))
