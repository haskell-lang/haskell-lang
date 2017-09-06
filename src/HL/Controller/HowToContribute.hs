{-# LANGUAGE OverloadedStrings #-}

-- | How to contribute page controller.

module HL.Controller.HowToContribute where

import HL.Controller
import HL.Controller.Markdown
import HL.View

-- | HowToContribute controller.
getHowToContributeR :: C (Html ())
getHowToContributeR =
  markdownPage
               "How to contribute"
               "how-to-contribute.md"
