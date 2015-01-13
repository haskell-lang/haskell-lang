{-# LANGUAGE OverloadedStrings #-}

-- | Report page view.

module HL.View.Report where

import HL.View
import HL.View.Template

-- | Report view.
reportV :: Int -> FilePath -> Html () -> FromLucid App
reportV year _ inner =
  template [DocumentationR,ReportHomeR year]
           "Report"
           (\_ ->
              container_ (row_ (span12_ [class_ "col-md-12"] inner)))
