{-# LANGUAGE OverloadedStrings #-}

-- | IRC channels.

module HL.Controller.Irc where

import HL.Controller
import HL.Controller.Markdown
import HL.View

-- | List IRC places.
getIrcR :: C (Html ())
getIrcR =
  markdownPage [CommunityR,IrcR] "IRC Channels" "irc.md"
