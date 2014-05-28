{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Templates.

module HL.V.Template where

import HL.V hiding (item)

import Data.Monoid
import Data.Text (pack)
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
    (\cur url ->
       div [class_ "template"]
           (do navigation True cur url
               container (bread url crumbs)
               inner url))

-- | Render the basic site skeleton.
skeleton
  :: Text
  -> FromSenza App
  -> FromSenza App
skeleton ptitle inner mroute url =
  docTypeHtml
    (do head [] headinner
        body (maybe []
                    (\route -> [class_ (toValue ("page-" <> routeToSlug route))])
                    mroute)
             bodyinner)
  where
    headinner =
      do headtitle (toHtml ptitle)
         meta [charset "utf-8"]
         meta [httpEquiv "X-UA-Compatible",content "IE edge"]
         meta [name "viewport",content "width=device-width, initial-scale=1"]
         link [rel "stylesheet"
              ,type_ "text/css"
              ,href "http://fonts.googleapis.com/css?family=Open+Sans"]
         styles url
                [StaticR css_bootstrap_min_css
                ,StaticR css_haskell_font_css
                ,StaticR css_hscolour_css
                ,StaticR css_hl_css]
    bodyinner =
      do div [class_ "wrap"]
             (inner mroute url)
         div [class_ "footer"]
             (div [class_ "container"]
                  (p [] (do "Copyright © 2014 haskell-lang.org")))
         scripts url
                 [js_jquery_js
                 ,js_jquery_cookie_js
                 ,js_bootstrap_min_js
                 ,js_warp_reload_js]

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
           link [rel "stylesheet"
                ,type_ "text/css"
                ,href (url route)])

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
                    (toHtml (routeToHuman route)))
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
                     (toHtml (routeToHuman route)))))

-- | Generate a human-readable string from a route.
routeToHuman :: Route App -> Text
routeToHuman r =
  case r of
    CommunityR     -> "Community"
    IrcR           -> "IRC"
    DocumentationR -> "Documentation"
    HomeR          -> "Home"
    ReloadR        -> "Reload"
    MailingListsR  -> "Mailing Lists"
    NewsR          -> "News"
    StaticR{}      -> "Static"
    DownloadsR     -> "Downloads"
    WikiR t        -> "Wiki: " <> t
    ReportR i _    -> "Report " <> pack (show i)
    ReportHomeR i  -> "Report " <> pack (show i)
    WikiHomeR{}    -> "Wiki"

-- | Generate a slug string from a route.
routeToSlug :: Route App -> Text
routeToSlug r =
  case r of
    CommunityR     -> "community"
    IrcR           -> "irc"
    DocumentationR -> "documentation"
    HomeR          -> "home"
    ReloadR        -> "reload"
    MailingListsR  -> "mailing-lists"
    NewsR          -> "news"
    StaticR{}      -> "static"
    DownloadsR     -> "downloads"
    WikiR{}        -> "wiki"
    ReportR{}      -> "report"
    ReportHomeR{}  -> "report"
    WikiHomeR{}    -> "wiki"

-- | Set the background image for an element.
background :: (Route App -> AttributeValue) -> Route Static -> Attribute
background url route =
  style ("background-image: url(" <> url (StaticR route) <> ")")
