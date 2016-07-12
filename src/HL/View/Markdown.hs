{-# LANGUAGE OverloadedStrings #-}

-- | Simple markdown page view.

module HL.View.Markdown where

import HL.View
import HL.View.Template

-- | Render a simple page.
markdownV :: Text -> View App () -> View App ()
markdownV t inner =
  template t (container_ (row_ (span12_ [class_ "col-md-12"] inner)))
