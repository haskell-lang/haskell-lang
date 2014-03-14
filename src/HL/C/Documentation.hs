{-# LANGUAGE OverloadedStrings #-}

-- | Documentation page controller.

module HL.C.Documentation where

import HL.C.Markdown
import HL.C

-- | Documentation controller.
getDocumentationR :: C Html
getDocumentationR =
  markdownPage [DocumentationR] "Documentation" "documentation.md"
