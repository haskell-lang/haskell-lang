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
  :: Text
  -> View App ()
  -> View App ()
template ptitle inner =
  templateWithBodyEnder ptitle
                        inner
                        (return ())

-- | Render a template, with some additional content just before
-- </body>.
templateWithBodyEnder :: Text
                      -> View App ()
                      -> View App ()
                      -> View App ()
templateWithBodyEnder ptitle inner bodyender =
  skeleton ptitle
           (return ())
           (div_ [class_ "template"]
                 (do navigation True
                     container_ bread
                     inner))
           bodyender

-- | Render the basic site skeleton.
skeleton
  :: Text
  -> View App ()
  -> View App ()
  -> View App ()
  -> View App ()
skeleton ptitle innerhead innerbody bodyender =
  doctypehtml_
    (do head_ headinner
        mroute <- lift (asks pageRoute)
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
         url <- lift (asks pageRender)
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

         innerhead
    bodyinner =
      do div_ [class_ "wrap"] innerbody
         footer
         scripts
                 [js_jquery_js
                 ,js_bootstrap_min_js
                 ,js_home_js]
         bodyender
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
scripts :: [Route Static] -> View App ()
scripts routes = do
    url <- lift (asks pageRender)
    mapM_
        (\route ->
              script_ [src_ (url (StaticR route))] (mempty :: Text))
        routes

-- | Make a list of style links.
styles :: (a -> Text) -> [a] -> View App ()
styles url =
  mapM_ (\route ->
           linkcss (url route))

-- | A link to CSSxs
linkcss :: Text -> View App ()
linkcss uri =
  link_ [rel_ "stylesheet",type_ "text/css",href_ uri]

-- | Main navigation.
navigation :: Bool -> View App ()
navigation showBrand =
  nav_ [class_ "navbar navbar-default"]
       (div_ [class_ "container"]
             (do when showBrand brand
                 items))
  where items =
          div_ [class_ "collapse navbar-collapse"]
               (ul_ [class_ "nav navbar-nav"]
                    (mapM_ item [GetStartedR,LibrariesR,DocumentationR,CommunityR]))
          where item :: Route App -> View App ()
                item route =
                  do mroute <- lift (asks pageRoute)
                     url <- lift (asks pageRender)
                     crumbs <- lift (asks pageCrumbs)
                     li_ [class_ "active" | Just route == mroute || elem route (map fst crumbs)]
                         (a_ [href_ (url route)]
                             (toHtml (toHuman route)))
        brand =
          do url <- lift (asks pageRender)
             div_ [class_ "navbar-header"]
                  (a_ [class_ "navbar-brand",href_ (url HomeR)]
                      (do logo
                          "Haskell"))

-- | The logo character in the right font. Style it with an additional
-- class or wrapper as you wish.
logo :: Monad m => HtmlT m ()
logo = span_ [class_ "logo"] "\57344"

-- | Breadcrumb.
bread :: View App ()
bread =
  do crumbs <- lift (asks pageCrumbs)
     url <- lift (asks pageRender)
     unless (length crumbs == 1)
            (ol_ [class_ "breadcrumb"]
                 (forM_ crumbs
                        (\(route,title) ->
                           li_ (a_ [href_ (url route)]
                                   (toHtml title)))))

-- | Set the background image for an element.
background :: (Route App -> Text) -> Route Static -> Attribute
background url route =
  style_ ("background-image: url(" <> url (StaticR route) <> ")")

-- | Footer across the whole site.
footer :: View App ()
footer =
  div_ [class_ "footer"]
       (div_ [class_ "container"]
             (p_  hlCopy))
  where hlCopy :: View App ()
        hlCopy =
          do span_ [class_ "item"] "\169 2014-2016 haskell-lang.org"
             span_ [class_ "item footer-contribute pull-right"]
                   (do "Got changes to contribute? "
                       a_ [href_ "https://github.com/haskell-lang/haskell-lang"] "Fork or comment on Github")

defaultLayoutImpl :: Y.WidgetT App IO () -> Y.HandlerT App IO a
defaultLayoutImpl widget = do
  pc <- Y.widgetToPageContent widget
  render <- Y.getUrlRenderParams
  let title = TL.toStrict (Blaze.renderHtml (Y.pageTitle pc))
      body = Y.pageBody pc
      content :: View App ()
      content = template title
              (container_
                 (row_ (span12_ [class_ "col-md-12"]
                   (toHtmlRaw (BlazeUtf8.renderHtml (body render))))))
  lucid content >>= Y.sendResponse

-- | Make an element that looks like an OS X window.
osxWindow :: Text -> View App () -> View App ()
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
