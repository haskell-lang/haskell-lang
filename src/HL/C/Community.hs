{-# LANGUAGE OverloadedStrings #-}

-- | Community page controller.

module HL.C.Community where

import HL.C.Markdown
import HL.C

-- | Community controller.
getCommunityR :: C Html
getCommunityR =
  markdownPage CommunityR "Community" "community.md"
