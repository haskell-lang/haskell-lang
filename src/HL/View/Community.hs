{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.View.Community where

import HL.View
import HL.View.Template

-- | Community view.
communityV :: FromLucid App
communityV =
  template
    []
    "Community"
    (\url ->
       container_
         (row_
            (span12_
               (do h1_ "Community"
                   p_
                     "The Haskell community is spread out across several mediums \
                     \and around the world!"
                   h2_ "Online Communities"
                   p_ "Haskellers are active on a number of online areas, but the most busy are below:"
                   ul_ (online url)
                   h2_ "Offline Communities"
                   p_ "There are a number of offline Haskell communities where haskellers meet to learn and code. Some are listed below:"
                   ul_ offline
                   h2_ ""))))

online :: (Route App -> Text) -> Html ()
online url =
  do li_ (a_ [href_ (url MailingListsR)] "The Haskell mailing lists")
     li_ (a_ [href_ (url IrcR)] "IRC (online chat)")
     li_ (a_ [href_ "http://stackoverflow.com/questions/tagged?tagnames=haskell"] "StackOverflow")
     li_ (a_ [href_ "https://plus.google.com/communities/104818126031270146189"] "Google+ community")
     li_ (a_ [href_ "http://www.reddit.com/r/haskell"] "Reddit")
     li_ (a_ [href_ "http://www.haskell.org/haskellwiki/Haskell"] "Wiki")
     li_ (a_ [href_ "http://planet.haskell.org/"] "The blogosphere")

offline :: Html ()
offline =
  do li_ (a_ [href_ "https://www.haskell.org/haskell-symposium/"] "The Haskell Symposium")
     li_ (a_ [href_ "http://www.meetup.com/Bay-Area-Haskell-Users-Group/"] "Bay Area Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Boston-Haskell/"] "Boston Haskell")
     li_ (a_ [href_ "http://www.meetup.com/berlinhug/"] "Berlin Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/NY-Haskell/"] "New York Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/find/?allMeetups=true&keywords=Haskell&radius=Infinity"] "More Haskell meetups at meetup.com")
