{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | News page view.

module HL.View.News where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | News view.
newsV :: Blaze App
newsV =
  template
    [(NewsR,"News")]
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "News"
                   p []
                     "Insert news here."))))
