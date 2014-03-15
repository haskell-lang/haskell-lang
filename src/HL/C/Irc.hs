{-# LANGUAGE OverloadedStrings #-}

-- | IRC channels.

module HL.C.Irc where

import HL.C.Markdown
import HL.C

-- | List IRC places.
getIrcR :: C Html
getIrcR =
  markdownPage [CommunityR,IrcR] "IRC Channels" "irc.md"
