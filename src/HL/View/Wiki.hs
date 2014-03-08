{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Wiki page view.

module HL.View.Wiki where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Wiki view.
wikiV :: Blaze App
wikiV =
  template
    [(WikiR "","Wiki")]
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "Wiki"
                   p []
                     "Insert wiki here."))))
