{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.V.Community where

import HL.V
import HL.V.Template

-- | Community view.
communityV :: Blaze App
communityV =
  template
    [CommunityR]
    "Community"
    (\url ->
       container
         (row
            (span12
               (do h1 [] "Community"
                   p []
                     "The Haskell community is spread out online across several mediums \
                     \and around the world!"
                   ul []
                      (do li [] (a [href (url MailingListsR)] "The Haskell mailing lists")
                          li [] "StackOverflow"
                          li [] "G+"
                          li [] "Reddit"
                          li [] "The Wiki")))))
