{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | News page view.

module HL.V.News where

import HL.V
import HL.V.Template

-- | News view.
newsV :: Blaze App
newsV =
  template
    [NewsR]
    "News"
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "News"
                   p []
                     "Insert news here."))))
