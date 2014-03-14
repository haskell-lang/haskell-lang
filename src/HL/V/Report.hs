{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Report page view.

module HL.V.Report where

import HL.V
import HL.V.Template

-- | Report view.
reportV :: Blaze App
reportV =
  template
    [ReportR 2010]
    "Report"
    (\_ ->
       container
         (row
            (span12
               (do p [] "Insert report here."))))
