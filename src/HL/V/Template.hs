{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Templates.

module HL.V.Template where

import           HL.V hiding (item)

import qualified Blaze.Elements as E

-- | Render a template.
template
  :: [(Route App,Text)]
  -> Text
  -> ((Route App -> AttributeValue) -> Html)
  -> Blaze App
template crumbs ptitle inner cur url =
  docTypeHtml
    (do head []
             (do headtitle (toHtml ptitle)
                 meta [charset "utf-8"]
                 meta [httpEquiv "X-UA-Compatible",content "IE edge"]
                 meta [name "viewport",content "width=device-width, initial-scale=1"]
                 link [rel "stylesheet"
                      ,type_ "text/css"
                      ,href "http://fonts.googleapis.com/css?family=Open+Sans"]
                 styles [StaticR css_bootstrap_min_css
                        ,StaticR css_haskell_font_css
                        ,StaticR css_hscolour_css
                        ,ThemeR])
        body []
             (do div [class_ "wrap"]
                     (do navigation cur url
                         container (bread url crumbs)
                         inner url)
                 div [class_ "footer"]
                     (div [class_ "container"]
                          (p [] (do "Copyright © 2014 haskell-lang.org")))
                 scripts [js_jquery_js
                         ,js_jquery_cookie_js
                         ,js_bootstrap_min_js
                         ,js_warp_reload_js]))
  where
    scripts =
      mapM_ (\route ->
               script [src (url (StaticR route))]
                      (return ()))
    styles =
      mapM_ (\route ->
               link [rel "stylesheet"
                    ,type_ "text/css"
                    ,href (url route)])

-- | Main navigation.
navigation :: Blaze App
navigation cur url =
  nav [class_ "navbar navbar-default"]
      (div [class_ "container"]
           (do brand
               items))
  where
    items =
      div [class_ "collapse navbar-collapse"]
          (ul [class_ "nav navbar-nav"]
              (mapM_ (uncurry item)
                     [(DownloadsR,"Downloads")
                     ,(CommunityR,"Community")
                     ,(DocumentationR,"Documentation")
                     ,(NewsR,"News")
                     ,(WikiHomeR,"Wiki")]))
      where item route t = li theclass (a [href (url route)] t)
              where theclass
                      | Just route == cur = [class_ "active"]
                      | otherwise = []
    brand =
      div [class_ "navbar-header"]
           (do a [class_ "navbar-brand"
                 ,href (url HomeR)]
                 (do span [class_ "logo"]
                          "\xe000"
                     "Haskell"))

-- | Breadcrumb.
bread :: (t -> E.AttributeValue) -> [(t,Text)] -> Html
bread url crumbs =
  ol [class_ "breadcrumb"]
     (forM_ crumbs
            (\(route,t) ->
               li [] (a [href (url route)]
                        (toHtml t))))
