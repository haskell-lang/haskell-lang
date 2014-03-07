{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Documentation page view.

module HL.View.Documentation where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Documentation view.
documentationV :: Blaze App
documentationV =
  template
    [(DocumentationR,"Documentation")]
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "Documentation"
                   h2 [] "Online Resources"
                   p [] "Some stuff here."))))
