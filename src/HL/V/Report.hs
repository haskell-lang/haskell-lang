{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Report page view.

module HL.V.Report where

import HL.V
import HL.V.Template

-- | Report view.
reportV :: Int -> FilePath -> Html -> Blaze App
reportV year page inner =
  template
    [DocumentationR
    ,ReportHomeR year]
    "Report"
    (\_ ->
       container
         (row
            (span12
               inner)))
