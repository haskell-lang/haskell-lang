{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import qualified Data.Map as Map
import HL.Controller
import HL.View
import HL.View.Markdown
import HL.Model.Markdown

-- | Get mailing lists.
getTutorialR :: Text -> C (Html ())
getTutorialR slug = do
    tutorials <- fmap appTutorials getYesod
    tutorial <- maybe notFound return (Map.lookup slug tutorials)

    let title = tutorialTitle tutorial
        !html = renderMarkdown (tutorialContent tutorial)
    lucid (markdownV title html)
