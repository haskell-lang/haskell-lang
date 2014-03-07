{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Templates.

module HL.View.Template where

import HL.Foundation

import Blaze.Elements as E
import Blaze.Attributes as A
import Blaze.Prelude
import Blaze.Bootstrap

-- | Render a template.
template :: Blaze App -- ^ Content.
         -> Blaze App
template inner url =
  docTypeHtml
    (do head
          (do E.title "Haskell"
              with meta [charset "utf-8"]
              with meta [httpEquiv "X-UA-Compatible",A.content "IE edge"]
              with meta [name "viewport",A.content "width=device-width, initial-scale=1"]
              styles [css_bootstrap_min_css
                     ,css_bootstrap_theme_min_css])
        body
          (do with div
                   [class_ "wrap"]
                   (do inner url
                       with div
                            [class_ "footer"]
                            (container
                               (row
                                  (span12
                                     (with div
                                           [class_ "muted credit"]
                                           "Footer")))))
              scripts [js_jquery_js
                      ,js_jquery_cookie_js
                      ,js_bootstrap_min_js]))
  where
    scripts =
      mapM_ (\route ->
               with script
                    [src (url (StaticR route))]
                    (return ()))
    styles =
      mapM_ (\route ->
               with link
                    [rel "stylesheet"
                    ,type_ "text/css"
                    ,href (url (StaticR route))])
