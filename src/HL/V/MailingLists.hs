-- | Mailing lists page.

module HL.V.MailingLists where

import HL.V
import HL.V.Template

-- | Show mailing lists available.
mailingListsV :: Blaze App
mailingListsV =
  markdownPage [CommunityR,MailingListsR] "Mailing Lists" "mailing-lists"
