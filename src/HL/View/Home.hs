{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home/landing page.

module HL.View.Home where

import HL.Types
import HL.View
import HL.View.Code
import HL.View.Home.Features
import HL.View.Home.Whyhaskell
import HL.View.Template
import System.Random

-- | Home view.
homeV :: SnippetInfo -> View App ()
homeV snippets =
    skeleton
        "Haskell Language"
        (linkcss "https://fonts.googleapis.com/css?family=Ubuntu:700")
        (do url <- lift (asks pageRender)
            navigation True
            header snippets url
            whyhaskell
            when False (community url)
            when False features
            when False (try url)
            sponsors
            events
            div_ [class_ "mobile"] $ (navigation False))
        (scripts
             [js_jquery_console_js, js_tryhaskell_js, js_tryhaskell_pages_js])

-- | Top header section with the logo and code sample.
header :: SnippetInfo -> (Route App -> Text) -> View App ()
header snippetInfo url =
  div_ [class_ "header"] $
  (container_
     (row_ (do span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do branding
                                summation))
               span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do tag
                                snippet)))))
  where branding = span_ [class_ "name", background url img_logo_png] "Haskell"
        summation =
          span_ [class_ "summary"] "An advanced purely-functional programming language"
        tag = span_ [class_ "tag"] "Declarative, statically typed code."
        snippet =
          maybe (p_ (toHtml (show index)))
                sample
                (lookup index
                        (zip [0 ..]
                             (siSnippets snippetInfo)))
        index =
          fst (randomR (0, (length (siSnippets snippetInfo)) - 1)
                       (mkStdGen (siSeed snippetInfo)))

-- | Show a sample code.

sample :: Snippet -> View App ()
sample snippet =
  div_ [class_ "code-sample", title_ (snippetTitle snippet)]
       (haskellPre (snippetCode snippet))

-- | Try Haskell section.
try :: (Route App -> Text) -> View App ()
try _ =
  div_ [class_ "try", onclick_ "tryhaskell.controller.inner.click()"]
       (container_
          (row_ (do span6_ [class_ "col-md-6"] repl
                    span6_ [class_ "col-md-6", id_ "guide"]
                           (return ()))))
  where repl =
          do h2_ "Try it"
             noscript_ (span6_ (div_ [class_ "alert alert-warning"]
                    "Try Haskell requires JavaScript to be enabled."))
             span6_ [hidden_ "", id_ "cookie-warning"]
                  (div_ [class_ "alert alert-warning"]
                  "Try Haskell requires cookies to be enabled.")
             div_ [id_ "console"]
                  (return ())

-- | Community section.
-- TOOD: Should contain a list of thumbnail videos. See mockup.
community :: (Route App -> Text) -> View App ()
community url =
  div_ [id_ "community-wrapper"]
       (do div_ [class_ "community", background url img_community_jpg]
                (do container_
                      [id_ "tagline"]
                      (row_ (span8_ [class_ "col-md-8"]
                                    (do h1_ "An open source community effort for over 20 years"
                                        p_ [class_ "learn-more"]
                                           (a_ [href_ (url CommunityR)] "Learn more"))))))

-- | Events section.
-- TODO: Take events section from Haskell News?
events :: View App ()
events =
  return ()

-- | List of sponsors.
sponsors :: View App ()
sponsors =
    div_ [class_ "sponsors"] $
    container_ $
    do row_ (span6_ [class_ "col-md-6"] (h1_ "Sponsors"))
       row_
           (do span6_
                   [class_ "col-md-6"]
                   (p_
                        (do strong_
                                (a_
                                     [href_ "https://www.fpcomplete.com/"]
                                     "FP Complete")
                            " The leading commercial provider of Haskell consulting"))
               span6_
                   [class_ "col-md-6"]
                   (p_
                        (do strong_
                                (a_
                                     [href_ "http://haskellbook.com/"]
                                     "Haskell Programming from First Principles")
                            " Think learning Haskell is difficult? It doesn't have to be.")))
