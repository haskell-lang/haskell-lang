{-# LANGUAGE OverloadedStrings #-}

-- | Mailing lists page.

module HL.Controller.SuccessStories where

import HL.Controller
import HL.Controller.Markdown
import HL.View

-- | Get success stories.
getSuccessStoriesR :: C (Html ())
getSuccessStoriesR =
  markdownPage
               "Success Stories"
               "success-stories.md"
