{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.V.Community where

import HL.V hiding (list)
import HL.V.Template

-- | Community view.
communityV :: FromBlaze App
communityV =
  template
    []
    "Community"
    (\url ->
       container
         (row
            (span12
               (do h1 "Community"
                   p
                     "The Haskell community is spread out across several mediums \
                     \and around the world!"
                   h2 "Online Communities"
                   p "Haskellers are active on a number of online areas, but the most busy are below:"
                   ul (online url)
                   h2 "Offline Communities"
                   p "There are a number of offline Haskell communities where haskellers meet to learn and code. Some are listed below:"
                   ul offline
                   h2 ""))))

online :: (Route App -> AttributeValue) -> Html
online url =
  do li (a ! href (url MailingListsR)$ "The Haskell mailing lists")
     li (a ! href (url IrcR)$ "IRC (online chat)")
     li (a ! href "http://stackoverflow.com/questions/tagged?tagnames=haskell"$ "StackOverflow")
     li (a ! href "https://plus.google.com/communities/104818126031270146189"$ "Google+ community")
     li (a ! href "http://www.reddit.com/r/haskell"$ "Reddit")
     li (a ! href "http://www.haskell.org/haskellwiki/Haskell"$ "Wiki")
     li (a ! href "http://planet.haskell.org/"$ "The blogosphere")

offline :: Html
offline =
  do li (a ! href "https://www.haskell.org/haskell-symposium/"$ "The Haskell Symposium")
     li (a ! href "http://www.meetup.com/Bay-Area-Haskell-Users-Group/"$ "Bay Area Haskell Users Group")
     li (a ! href "http://www.meetup.com/Boston-Haskell/"$ "Boston Haskell")
     li (a ! href "http://www.meetup.com/berlinhug/"$ "Berlin Haskell Users Group")
     li (a ! href "http://www.meetup.com/NY-Haskell/"$ "New York Haskell Users Group")
     li (a ! href "http://www.meetup.com/find/?allMeetups=true&keywords=Haskell&radius=Infinity"$ "More Haskell meetups at meetup.com")
