{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.View.Community where

import HL.View
import HL.View.Template

-- | Community view.
communityV :: View App ()
communityV =
  template
    "Community"
    (do
       url <- lift (asks pageRender)
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Community"
                   p_
                     "Haskellers interact, talk and collaborate through several mediums \
                     \ around the world. There are places to learn, to teach, to ask questions, and to find contributors and collaborators."
                   h2_ "Online Communities and Social Resources"
                   p_ "Haskellers are active on a number of online areas, but the most busy are below:"
                   ul_ (online url)
                   h2_ "In-person Groups and Meetups"
                   p_ "There are a number of Haskell Users groups where haskellers meet to learn and code. Some are listed below:"
                   ul_ offline
                   h2_ "Conferences and Events"
                   p_ "There are a number of conferences and events featuring Haskell, some focusing on the academic side of things, and some on the commercial or hobbyist side. Here are just a few:"
                   h3_ "Academic Conferences"
                   ul_ academicConferences
                   h3_ "Non-Academic Conferences"
                   ul_ commercialConferences
                   h3_ "Hackathons"
                   p_ "Haskell Hackathons are a long tradition, with lots of learning and social exchange. In many ways they function as semi-structured conferences. Here are some of the most notable:"
                   ul_ hackathons
                   h2_ "Specific Interest Groups"
                   ul_ sigs
                   h2_ ""))))

online :: (Route App -> Text) -> View App ()
online url =
  do li_ (a_ [href_ (url MailingListsR)] "The Haskell mailing lists")
     li_ (a_ [href_ (url IrcR)] "IRC (online chat)")
     li_ (a_ [href_ (url SuccessStoriesR)] "Haskell success stories")
     li_ (a_ [href_ (url HowToContributeR)] "How to contribute to Haskell projects")
     li_ (a_ [href_ "http://stackoverflow.com/questions/tagged?tagnames=haskell"] "StackOverflow")
     li_ (a_ [href_ "https://plus.google.com/communities/104818126031270146189"] "Google+ community")
     li_ (a_ [href_ "https://www.facebook.com/groups/programming.haskell/"] "Facebook community")
     li_ (a_ [href_ "http://www.reddit.com/r/haskell"] "Reddit")
     li_ (a_ [href_ "http://www.haskell.org/haskellwiki/Haskell"] "Wiki")
     li_ (a_ [href_ "http://planet.haskell.org/"] "The blogosphere")
     li_ (a_ [href_ "https://haskellweekly.news"] "Haskell Weekly")

offline :: View App ()
offline =
  do li_ (a_ [href_ "http://www.meetup.com/ATX-Haskell/"] "Austin Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Bay-Area-Haskell-Users-Group/"] "Bay Area Haskell Users Group")
     li_ (a_ [href_ "https://www.meetup.com/eastbayfunctionalprogramming/"] "East Bay Functional Programming Study Group")
     li_ (a_ [href_ "http://www.meetup.com/Boston-Haskell/"] "Boston Haskell")
     li_ (a_ [href_ "http://www.meetup.com/berlinhug/"] "Berlin Haskell Users Group")
     li_ (a_ [href_ "http://ChicagoHaskell.com/"] "Chicago Haskell")
     li_ (a_ [href_ "http://www.meetup.com/NY-Haskell/"] "New York Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/London-Haskell/"] "London Haskell")
     li_ (a_ [href_ "http://www.meetup.com/seahug/"] "Seattle Area Haskell Users' Group")
     li_ (a_ [href_ "http://www.meetup.com/find/?allMeetups=true&keywords=Haskell&radius=Infinity"] "More Haskell meetups at meetup.com")

academicConferences :: View App ()
academicConferences =
  do li_ (a_ [href_ "https://www.haskell.org/haskell-symposium/"] "The Haskell Symposium")
     li_ (a_ [href_ "https://wiki.haskell.org/HaskellImplementorsWorkshop"] "Haskell Implementors' Workshop")
     li_ (a_ [href_ "http://www.icfpconference.org/"] "The International Conference on Functional Programming")
     li_ (a_ [href_ "http://popl.mpi-sws.org/"] "Symposium on Principles of Programming Languages")
     li_ (a_ [href_ "http://www.ifl-symposia.org/"] "International Symposia on Implementation and Application of Functional Languages")
     li_ (a_ [href_ "http://www.tifp.org/"] "Symposium on Trends in Functional Programming ")

commercialConferences :: View App ()
commercialConferences =
  do li_ (a_ [href_ "http://cufp.org/"] "Commercial Users of Functional Programming (Roving)")
     li_ (a_ [href_ "http://www.lambdajam.com/"] "LambdaJam (Chicago, IL, USA)")
     li_ (a_ [href_ "http://functionalconf.com/"] "Functional Conf (Bangalore, IN)")
     li_ (a_ [href_ "http://www.lambdacon.org/"] "LambdaCon (Bologna, IT)")
     li_ (a_ [href_ "http://bobkonf.de/"] "BOB (Berlin, DE)")
     li_ (a_ [href_ "http://www.iba-cg.de/hal9.html"] "HaL (Leipzig, Halle, DE)")
     li_ (a_ [href_ "https://skillsmatter.com/conferences/1907-haskell-exchange-2014"] "Skills Matter's Haskell eXchange (London, GB)")
     li_ (a_ [href_ "http://www.degoesconsulting.com/lambdaconf-2015/"] "LambdaConf (Boulder, CO, USA)")
     li_ (a_ [href_ "http://composeconference.org"] "Compose :: Conference (NY, NY, USA)")

hackathons :: View App ()
hackathons =
  do li_ (a_ [href_ "http://bayhac.org/"] "BayHac (Bay Area, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/Hac_Phi"] "Hac Phi (Philadelphia, PA, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/ZuriHac"] "ZuriHac (Zurich, CH)")

sigs :: View App ()
sigs =
  do li_ (a_ [href_ "http://industry.haskell.org/"] "Industrial Haskell Group")
     li_ (a_ [href_ "http://commercialhaskell.com/"] "Commercial Haskell Group")
     li_ (a_ [href_ "http://datahaskell.org"] "DataHaskell: data science, machine learning and scientific computation")
     li_ (a_ [href_ "http://lurk.org/groups/haskell-art/"] "Haskell Art")
