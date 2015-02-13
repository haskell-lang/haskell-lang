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
reportNodeV :: Mode -> Int -> Html () -> FromLucid App
reportNodeV mode year inner =
  templateWithBodyEnder
    [DocumentationR,ReportR,ReportModeR mode year]
    (T.pack (show year) <>
     " report")
    (const (container_ (row_ (span12_ [class_ "col-md-12"] inner))))
    (\_ url ->
       script_ [src_ (url (StaticR js_report_js))] "")

-- | Report types view.
reportV :: FromLucid App
reportV =
  template [DocumentationR,ReportR] "Report"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Report"
                   p_ "There are two ways of viewing the report, either:"
                   ul_ (do li_ (do a_ [href_ (url (ReportModeR Mono 2010))]
                                       "Mono"
                                   " - entirely on one web page")
                           li_ (do a_ [href_ (url (ReportModeR Node 2010))]
                                      "Node"
                                   " - with one web page per node"))))))
