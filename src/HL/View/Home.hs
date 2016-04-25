{-# LANGUAGE OverloadedStrings #-}

-- | Home/landing page.

module HL.View.Home where

import HL.View
import HL.View.Code
import HL.View.Home.Features
import HL.View.Template

-- | Home view.
homeV :: [(Text, Text, Text)] -> FromLucid App
homeV vids =
  skeleton
    "Haskell Language"
    (\_ _ ->
       linkcss "https://fonts.googleapis.com/css?family=Ubuntu:700")
    (\cur url ->
       do navigation True [] Nothing url
          header url
          try url
          community url vids
          features
          events
          div_ [class_ "mobile"] $
               (navigation False [] cur url))
    (\_ url ->
       scripts url
               [js_jquery_console_js
               ,js_tryhaskell_js
               ,js_tryhaskell_pages_js])

-- | Top header section with the logo and code sample.
header :: (Route App -> Text) -> Html ()
header url =
  div_ [class_ "header"] $
  (container_
     (row_ (do span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do branding
                                summation))
               span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do tag
                                sample)))))
  where branding =
          span_ [class_ "name",background url img_logo_png] "Haskell"
        summation =
          span_ [class_ "summary"] "An advanced purely-functional programming language"
        tag =
          span_ [class_ "tag"] "Declarative, statically typed code."
        sample =
          div_ [class_ "code-sample",title_ "This example is contrived in order to demonstrate what Haskell looks like, including: (1) where syntax, (2) enumeration syntax, (3) pattern matching, (4) consing as an operator, (5) list comprehensions, (6) infix functions. Don't take it seriously as an efficient prime number generator."]
               (haskellPre codeSample)

-- | Code sample.
-- TODO: should be rotatable and link to some article.
codeSample :: Text
codeSample =
  "primes = filterPrime [2..] \n\
  \  where filterPrime (p:xs) = \n\
  \          p : filterPrime [x | x <- xs, x `mod` p /= 0]"

-- | Try Haskell section.
try :: (Route App -> Text) -> Html ()
try _ =
  div_ [class_ "try",onclick_ "tryhaskell.controller.inner.click()"]
       (container_
          (row_ (do span6_ [class_ "col-md-6"] repl
                    span6_ [class_ "col-md-6",id_ "guide"]
                           (return ()))))
  where repl =
          do h2_ "Try it"
             noscript_ (span6_ (div_ [class_ "alert alert-warning"]
                    "Try haskell requires Javascript to be enabled."))
             span6_ [hidden_ "", id_ "cookie-warning"]
                  (div_ [class_ "alert alert-warning"]
                  "Try haskell requires cookies to be enabled.")
             div_ [id_ "console"]
                  (return ())

-- | Community section.
-- TOOD: Should contain a list of thumbnail videos. See mockup.
community :: (Route App -> Text) -> [(Text, Text, Text)] -> Html ()
community url vids =
  div_ [id_ "community-wrapper"]
       (do div_ [class_ "community",background url img_community_jpg]
                (do container_
                      [id_ "tagline"]
                      (row_ (span8_ [class_ "col-md-8"]
                                    (do h1_ "An open source community effort for over 20 years"
                                        p_ [class_ "learn-more"]
                                           (a_ [href_ (url CommunityR)] "Learn more"))))
                    container_
                      [id_ "video-description"]
                      (row_ (span8_ [class_ "col-md-8"]
                                    (do h1_ (a_ [id_ "video-anchor"] "<title here>")
                                        p_ (a_ [id_ "video-view"] "View the video now \8594")))))
           div_ [class_ "videos"]
                (container_ (row_ (span12_ [class_ "col-md-12"]
                                           (ul_ (forM_ vids vid))))))
  where vid :: (Text,Text,Text) -> Html ()
        vid (n,u,thumb) =
          li_ (a_ [class_ "vid-thumbnail",href_ u,title_ n]
                  (img_ [src_ thumb]))

-- | Events section.
-- TODO: Take events section from Haskell News?
events :: Html ()
events =
  return ()
