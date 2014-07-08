{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Templates.

module HL.V.Template where

import HL.Types
import HL.V hiding (item)

import Data.Monoid
import Yesod.Static (Static)

-- | Render a template.
template
  :: [Route App]
  -> Text
  -> ((Route App -> AttributeValue) -> Html)
  -> FromSenza App
template crumbs ptitle inner =
  skeleton
    ptitle
    (\_ _ -> return ())
    (\cur url ->
       div [class_ "template"]
           (do navigation True cur url
               container (bread url crumbs)
               inner url))
    (\_ _ -> return ())

-- | Render the basic site skeleton.
skeleton
  :: Text
  -> FromSenza App
  -> FromSenza App
  -> FromSenza App
  -> FromSenza App
skeleton ptitle innerhead innerbody bodyender mroute url =
  docTypeHtml
    (do head [] headinner
        body (maybe []
                    (\route -> [class_ (toValue ("page-" <> toSlug route))])
                    mroute)
             (do bodyinner
                 analytics)
             )
  where
    headinner =
      do headtitle (toHtml ptitle)
         meta [charset "utf-8"]
         meta [httpEquiv "X-UA-Compatible",content "IE edge"]
         meta [name "viewport",content "width=device-width, initial-scale=1"]
         link [rel "shortcut icon",href (url (StaticR img_favicon_ico))]
         linkcss "https://fonts.googleapis.com/css?family=Open+Sans"
         styles url
                [StaticR css_bootstrap_min_css
                ,StaticR css_haskell_font_css
                ,StaticR css_hscolour_css
                ,StaticR css_hl_css]
         innerhead mroute url
    bodyinner =
      do div [class_ "wrap"]
             (innerbody mroute url)
         footer mroute
         scripts url
                 [js_jquery_js
                 ,js_bootstrap_min_js]
         bodyender mroute url
    -- TODO: pop this in a config file later.
    analytics =
      script []
             "var _gaq = _gaq || [];\n\
             \_gaq.push(['_setAccount', 'UA-15375175-1']);\n\
             \_gaq.push(['_trackPageview']);\n\
             \(function() {\n\
             \ var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
             \ ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
             \ var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
             \})();\n"

-- | Make a list of scripts.
scripts :: (Route App -> AttributeValue) -> [Route Static] -> Senza
scripts url =
  mapM_ (\route ->
           script [src (url (StaticR route))]
                  (return ()))

-- | Make a list of style links.
styles :: (a -> AttributeValue) -> [a] -> Senza
styles url =
  mapM_ (\route ->
           linkcss (url route))

-- | A link to CSSxs
linkcss :: AttributeValue -> Senza
linkcss uri =
  link [rel "stylesheet"
       ,type_ "text/css"
       ,href uri]

-- | Main navigation.
navigation :: Bool -> FromSenza App
navigation showBrand mroute url =
  nav [class_ "navbar navbar-default"]
      (div [class_ "container"]
           (do when showBrand brand
               items))
  where
    items =
      div [class_ "collapse navbar-collapse"]
          (ul [class_ "nav navbar-nav"]
              (mapM_ item
                     [DownloadsR
                     ,CommunityR
                     ,DocumentationR
                     ,NewsR]))
      where item route =
              li theclass
                 (a [href (url route)]
                    (toHtml (toHuman route)))
              where theclass
                      | Just route == mroute = [class_ "active"]
                      | otherwise = []
    brand =
      div [class_ "navbar-header"]
          (do a [class_ "navbar-brand"
                ,href (url HomeR)]
                (do logo
                    "Haskell"))

-- | The logo character in the right font. Style it with an additional
-- class or wrapper as you wish.
logo :: Senza
logo =
  span [class_ "logo"]
       "\xe000"

-- | Breadcrumb.
bread :: (Route App -> AttributeValue) -> [Route App] -> Html
bread url crumbs =
  ol [class_ "breadcrumb"]
     (forM_ crumbs
            (\route ->
               li []
                  (a [href (url route)]
                     (toHtml (toHuman route)))))

-- | Set the background image for an element.
background :: (Route App -> AttributeValue) -> Route Static -> Attribute
background url route =
  style ("background-image: url(" <> url (StaticR route) <> ")")

-- | Footer across the whole site.
footer :: Maybe (Route App) -> Senza
footer r =
  div [class_ "footer"]
      (div [class_ "container"]
           (p [] (do case r of
                       Just (WikiR page) -> wikiLicense (Just page)
                       Just (WikiHomeR{}) -> wikiLicense (Nothing :: Maybe Text)
                       _ -> hlCopy
                     "")))
  where
    hlCopy = do span [class_ "item"]
                    "Copyright © 2014 haskell-lang.org"
                span [class_ "item footer-contribute"]
                     (do "Got changes to contribute? "
                         a [href "https://github.com/chrisdone/hl"]
                           "Fork on Github" )
    wikiLicense page =
      do span [class_ "item"]
              wikiLink
         span [class_ "item"]
              (do "Wiki content is available under "
                  a [href "http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights"]
                    "a simple permissive license.")
      where
        wikiLink =
          case page of
            Nothing ->
              a [href "http://www.haskell.org/haskellwiki/"]
                "Go to haskell.org wiki"
            Just pn ->
              a [href (toValue ("http://www.haskell.org/haskellwiki/index.php?title=" <>
                                pn <>
                                "&action=edit"))]
                "Edit this page"
