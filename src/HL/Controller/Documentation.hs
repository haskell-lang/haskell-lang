{-# LANGUAGE OverloadedStrings #-}

-- | Documentation page controller.

module HL.Controller.Documentation where

import HL.Controller
import HL.View
import HL.View.Documentation

-- | Documentation controller.
getDocumentationR :: C (Html ())
getDocumentationR =
  lucid documentationV
