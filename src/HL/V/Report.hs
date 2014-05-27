{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Report page view.

module HL.V.Report where

import HL.V
import HL.V.Template

-- | Report view.
reportV :: Int -> FilePath -> Html -> FromSenza App
reportV year _ inner =
  template
    [DocumentationR
    ,ReportHomeR year]
    "Report"
    (\_ ->
       container
         (row
            (span12
               inner)))
