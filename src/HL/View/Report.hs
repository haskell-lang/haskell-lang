{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Report page view.

module HL.View.Report where

import           HL.Foundation
import           HL.View
import           HL.View.Template

import           Data.Monoid
import qualified Data.Text as T

-- | Report view.
reportNodeV :: Mode -> Int -> View App () -> View App ()
reportNodeV _mode year inner =
  templateWithBodyEnder

    (T.pack (show year) <>
     " report")
    ((container_ (row_ (span12_ [class_ "col-md-12"] inner))))
    (
       do url <- lift (asks pageRender)
          link_ [rel_ "stylesheet",type_ "text/css",href_ (url (StaticR css_report_css))]
          script_ [src_ (url (StaticR js_highlight_pack_js))] T.empty
          script_ [src_ (url (StaticR js_report_js))] T.empty)

-- | Report types view.
reportV :: View App ()
reportV =
  template  "Report"
    (container_
       (row_
          (span12_ [class_ "col-md-12"]
             (do h1_ "Report"
                 url <- lift (asks pageRender)
                 p_ "There are two ways of viewing the report, either:"
                 ul_ (do li_ (do a_ [href_ (url (ReportModeR Mono 2010))]
                                     "Mono"
                                 " - entirely on one web page")
                         li_ (do a_ [href_ (url (ReportModeR Node 2010))]
                                    "Node"
                                 " - with one web page per node"))))))
