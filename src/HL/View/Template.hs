{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Templates.

module HL.View.Template where

import HL.Types
import HL.View

import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Yesod.Core as Y
import Yesod.Static (Static)

-- | Render a template.
template
  :: [Route App]
  -> Text
  -> ((Route App -> Text) -> Html ())
  -> FromLucid App
template crumbs ptitle inner =
  templateWithBodyEnder crumbs
                        ptitle
                        inner
                        (\_ _ -> return ())

-- | Render a template, with some additional content just before
-- </body>.
templateWithBodyEnder :: [Route App]
                      -> Text
                      -> ((Route App -> Text) -> Html ())
                      -> FromLucid App
                      -> FromLucid App
templateWithBodyEnder crumbs ptitle inner bodyender =
  skeleton ptitle
           (\_ _ -> return ())
           (\cur url ->
              div_ [class_ "template"]
                   (do navigation True crumbs cur url
                       container_ (bread url crumbs)
                       inner url))
           bodyender

-- | Render the basic site skeleton.
skeleton
  :: Text
  -> FromLucid App
  -> FromLucid App
  -> FromLucid App
  -> FromLucid App
skeleton ptitle innerhead innerbody bodyender mroute url =
  doctypehtml_
    (do head_ headinner
        body_ [class_ ("page-" <> toSlug route) | Just route <- [mroute]]
              (do bodyinner
                  analytics))
  where
    headinner =
      do title_ (toHtml ptitle)
         meta_ [charset_ "utf-8"]
         meta_ [httpEquiv_ "X-UA-Compatible",content_ "IE edge"]
         meta_ [name_ "viewport",content_ "width=device-width, initial-scale=1"]
         meta_ [name_ "keywords",content_ "haskell,functional,pure,programming,lazy"]
         meta_ [name_ "description",
                content_ "The Haskell purely functional programming language home page."]
         link_ [rel_ "shortcut icon",href_ (url (StaticR img_favicon_ico))]
         linkcss "https://fonts.googleapis.com/css?family=Open+Sans"
         styles url
                [StaticR css_bootstrap_min_css
                ,StaticR css_haskell_font_css
                ,StaticR css_hscolour_css
                ,StaticR css_hl_css]
         link_
           [ href_ (url FeedR)
           , type_ "application/atom+xml"
           , rel_ "alternate"
           , title_ "Haskell Language News Feed"
           ]

         innerhead mroute url
    bodyinner =
      do div_ [class_ "wrap"] (innerbody mroute url)
         footer url mroute
         scripts url
                 [js_jquery_js
                 ,js_bootstrap_min_js
                 ,js_home_js]
         bodyender mroute url
    -- TODO: pop this in a config file later.
    analytics =
      script_ "var _gaq = _gaq || [];\n\
              \_gaq.push(['_setAccount', 'UA-51440536-1']);\n\
              \_gaq.push(['_trackPageview']);\n\
              \(function() {\n\
              \ var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
              \ ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
              \ var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
              \})();\n"

-- | Make a list of scripts.
scripts :: (Route App -> Text) -> [Route Static] -> Html ()
scripts url =
  mapM_ (\route ->
           script_ [src_ (url (StaticR route))] (mempty :: Text))

-- | Make a list of style links.
styles :: (a -> Text) -> [a] -> Html ()
styles url =
  mapM_ (\route ->
           linkcss (url route))

-- | A link to CSSxs
linkcss :: Text -> Html ()
linkcss uri =
  link_ [rel_ "stylesheet",type_ "text/css",href_ uri]

-- | Main navigation.
navigation :: Bool -> [Route App] -> FromLucid App
navigation showBrand crumbs mroute url =
  nav_ [class_ "navbar navbar-default"]
       (div_ [class_ "container"]
             (do when showBrand brand
                 items))
  where items =
          div_ [class_ "collapse navbar-collapse"]
               (ul_ [class_ "nav navbar-nav"]
                    (mapM_ item [GetStartedR,PackagesR,DocumentationR,CommunityR,NewsR]))
          where item :: Route App -> Html ()
                item route =
                  li_ [class_ "active" | Just route == mroute || elem route crumbs]
                      (a_ [href_ (url route)]
                          (toHtml (toHuman route)))
        brand =
          div_ [class_ "navbar-header"]
               (a_ [class_ "navbar-brand",href_ (url HomeR)]
                   (do logo
                       "Haskell"))

-- | The logo character in the right font. Style it with an additional
-- class or wrapper as you wish.
logo :: Html ()
logo = span_ [class_ "logo"] "\57344"

-- | Breadcrumb.
bread :: (Route App -> Text) -> [Route App] -> Html ()
bread url crumbs =
  ol_ [class_ "breadcrumb"]
      (forM_ crumbs
             (\route ->
                li_ (a_ [href_ (url route)]
                        (toHtml (toHuman route)))))

-- | Set the background image for an element.
background :: (Route App -> Text) -> Route Static -> Attribute
background url route =
  style_ ("background-image: url(" <> url (StaticR route) <> ")")

-- | Footer across the whole site.
footer :: (Route App -> Text) -> Maybe (Route App) -> Html ()
footer _ r =
  div_ [class_ "footer"]
       (div_ [class_ "container"]
             (p_ (case r of
                    Just (WikiR page) ->
                      wikiLicense (Just page)
                    Just (WikiHomeR{}) ->
                      wikiLicense (Nothing :: Maybe Text)
                    _ -> hlCopy)))
  where hlCopy =
          do span_ [class_ "item"] "\169 2014-2016 haskell-lang.org"
             span_ [class_ "item footer-contribute pull-right"]
                   (do "Got changes to contribute? "
                       a_ [href_ "https://github.com/haskell-lang/haskell-lang"] "Fork or comment on Github")
        wikiLicense :: Maybe Text -> Html ()
        wikiLicense page =
          do span_ [class_ "item"] wikiLink
             span_ [class_ "item"]
                   (do "Wiki content is available under "
                       a_ [href_ "http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights"]
                          "a simple permissive license.")
          where wikiLink =
                  case page of
                    Nothing ->
                      a_ [href_ "http://www.haskell.org/haskellwiki/"] "Go to haskell.org wiki"
                    Just pn ->
                      a_ [href_ ("http://www.haskell.org/haskellwiki/index.php?title=" <>
                                 pn <> "&action=edit")]
                         "Edit this page"

defaultLayoutImpl :: Y.WidgetT App IO () -> Y.HandlerT App IO a
defaultLayoutImpl widget = do
  pc <- Y.widgetToPageContent widget
  render <- Y.getUrlRenderParams
  let title = TL.toStrict (Blaze.renderHtml (Y.pageTitle pc))
      body = Y.pageBody pc
      content :: FromLucid App
      content = template [] title
              (\_ ->
                container_
                  (row_ (span12_ [class_ "col-md-12"]
                    (toHtmlRaw (BlazeUtf8.renderHtml (body render))))))
  lucid content >>= Y.sendResponse

-- | Make an element that looks like an OS X window.
osxWindow :: Text -> Html () -> Html ()
osxWindow title content =
  div_ [class_ "osx-window"]
       (div_ [class_ "window"]
             (do div_ [class_ "titlebar"]
                      (do div_ [class_ "buttons"]
                               (do div_ [class_ "closebtn"]
                                        (a_ [class_ "closebutton",href_ "#"]
                                            (span_ (strong_ "x")))
                                   div_ [class_ "minimize"]
                                        (a_ [class_ "minimizebutton",href_ "#"]
                                            (span_ (strong_ (toHtmlRaw ("&ndash;" ::Text)))))
                                   div_ [class_ "zoom"]
                                        (a_ [class_ "zoombutton",href_ "#"]
                                            (span_ (strong_ "+"))))
                          (span_ [class_ "title-bar-text"] (toHtml title)))
                 div_ [class_ "content"] content))
