{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Report page view.

module HL.View.Report where

import           HL.View
import           HL.View.Template

import           Data.Monoid
import qualified Data.Text as T

-- | Report view.
reportV :: Int -> FilePath -> Html () -> FromLucid App
reportV year _ inner =
  templateWithBodyEnder
    [DocumentationR,ReportHomeR year]
    (T.pack (show year) <>
     " report")
    (const (container_ (row_ (span12_ [class_ "col-md-12"] inner))))
    (\_ url ->
       script_ [src_ (url (StaticR js_report_js))] "")
