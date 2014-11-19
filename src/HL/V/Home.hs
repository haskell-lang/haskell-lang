{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Home/landing page.

module HL.V.Home where

import HL.V hiding (list)
import HL.V.Code
import HL.V.Home.Features
import HL.V.Template

-- | Home view.
homeV :: [(Text, Text, Text)] -> FromBlaze App
homeV vids =
  skeleton
    "Haskell Language"
    (\_ _ ->
       linkcss "https://fonts.googleapis.com/css?family=Ubuntu:700")
    (\cur url ->
       do navigation True Nothing url
          header url
          try url
          community url vids
          features
          sponsors
          events
          div ! class_ "mobile" $
              (navigation False cur url))
    (\_ url ->
       scripts url
               [js_jquery_console_js
               ,js_tryhaskell_js
               ,js_tryhaskell_pages_js])

-- | Top header section with the logo and code sample.
header :: (Route App -> AttributeValue) -> Html
header url =
  div ! class_ "header" $
      (container
         (row
            (do span6
                      (div ! class_ "branding" $
                           (do branding
                               summation))
                span6
                      (div ! class_ "branding" $
                           (do tag
                               sample)))))
  where branding =
          span !class_ "name"
               !background url img_logo_png$
               "Haskell"
        summation =
          span ! class_ "summary" $
               "An advanced purely-functional programming language"
        tag =
          span ! class_ "tag" $
               "Declarative, statically typed code."
        sample = div ! class_ "code-sample" $
                     (haskellPre codeSample)

-- | Code sample.
-- TODO: should be rotatable and link to some article.
codeSample :: Text
codeSample =
  "fibonacci 0 = 0\n\
  \fibonacci 1 = 1\n\
  \fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)"

-- | Try Haskell section.
try :: (Route App -> AttributeValue) -> Html
try _ =
  div ! class_ "try"
      ! onclick "tryhaskell.controller.inner.click()"
      $
      (container
         (row
            (do span6 repl
                span6 ! id "guide" $ (return ()))))
  where
    repl =
      do h2 "Try it"
         div ! id "console" $ (return ())

-- | Community section.
-- TOOD: Should contain a list of thumbnail videos. See mockup.
community :: (Route App -> AttributeValue) -> [(Text, Text, Text)] -> Html
community url vids =
  div !# "community-wrapper" $
    do div ! class_ "community"
           ! background url img_community_jpg $
         do container !# "tagline"  $ row $ span8 $ do
              h1 "An open source community effort for over 20 years"
              p ! class_ "learn-more" $
                a ! href (url CommunityR) $
                  "Learn more"
            container !# "video-description"  $ row $ span8 $ do
              h1 $ a !# "video-anchor" $
                "<title here>"
              p $ a !# "video-view" $
                "View the video now →"
       div ! class_ "videos" $
           (container (row (span12 (ul (forM_ vids vid)))))
  where
    vid (n,u,thumb) =
      li $
        a !. "vid-thumbnail" ! href (toValue u) ! title (toValue n) $
          img ! src (toValue thumb)

-- | Events section.
-- TODO: Take events section from Haskell News?
events :: Html
events =
  return ()

-- | List of sponsors.
sponsors :: Html
sponsors =
  div !. "sponsors" $
    container $
      do row (span6 (h1 "Sponsors"))
         row (do span6 (p (do strong (a ! href "https://www.datadoghq.com" $ "DataDog")
                              " provides powerful, customizable 24/7 metrics and monitoring \
                              \integration for all of Haskell.org, and complains loudly for \
                              \us when things go wrong."))
                 span6 (p (do strong (a ! href "https://www.fastly.com" $ "Fastly")
                              "'s Next Generation CDN provides low latency access for all of \
                              \Haskell.org's downloads and highest traffic services, including \
                              \the primary Hackage server, Haskell Platform downloads, and more." )))
         row (do span6 (p (do strong (a ! href "https://www.rackspace.com" $ "Rackspace")
                              " provides compute, storage, and networking resources, powering \
                              \almost all of Haskell.org in several regions around the world."))
                 span6 (p (do strong (a ! href "https://www.status.io" $ "Status.io")
                              " powers "
                              a ! href "https://status.haskell.org" $ "https://status.haskell.org"
                              ", and lets us easily tell you \
                              \when we broke something." )))
         row (do span6 (p (do strong (a ! href "http://www.galois.com" $ "Galois")
                              " provides infrastructure, funds, administrative resources and \
                              \has historically hosted critical Haskell.org infrastructure, \
                              \as well as helping the Haskell community at large with their work." )))
