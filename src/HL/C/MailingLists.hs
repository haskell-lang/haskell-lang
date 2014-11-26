{-# LANGUAGE OverloadedStrings #-}

-- | Mailing lists page.

module HL.C.MailingLists where

import HL.C
import HL.C.Markdown
import HL.V

-- | Get mailing lists.
getMailingListsR :: C (Html ())
getMailingListsR =
  markdownPage [CommunityR,MailingListsR]
               "Mailing Lists"
               "mailing-lists.md"
