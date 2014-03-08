{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Report page view.

module HL.View.Report where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Report view.
reportV :: Blaze App
reportV =
  template
    [(ReportR 2010,"Report")]
    (\url ->
       container
         (row
            (span12
               (do p [] "Insert report here."))))
