{-# LANGUAGE OverloadedStrings #-}

-- | Documentation page controller.

module HL.C.Documentation where

import HL.C
import HL.V.Documentation

-- | Documentation controller.
getDocumentationR :: C Html
getDocumentationR =
  blaze documentationV
