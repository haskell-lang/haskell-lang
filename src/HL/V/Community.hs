{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.V.Community where

import HL.V hiding (list)
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
                   h2 [] "Online Communities"
                   p [] "Haskellers are active on a number of online areas, but the most busy are below:"
                   ul [] (list url)
                   h2 [] ""))))

list url =
  do li [] (a [href (url MailingListsR)] "The Haskell mailing lists")
     li [] (a [href (url IrcR)] "IRC (online chat)")
     li [] (a [href "http://stackoverflow.com/questions/tagged?tagnames=haskell"] "StackOverflow")
     li [] (a [href "https://plus.google.com/communities/104818126031270146189"] "Google+ community")
     li [] (a [href "http://www.reddit.com/r/haskell"] "Reddit")
     li [] (a [href "http://www.haskell.org/haskellwiki/Haskell"] "Wiki")
