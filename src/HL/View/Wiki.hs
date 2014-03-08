{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Wiki page view.

module HL.View.Wiki where

import HL.Foundation
import HL.View.Template

import Blaze.Bootstrap
import Blaze.Prelude
import Data.Text (Text)

-- | Wiki view.
wikiV :: Either Text (Text,Html) -> Blaze App
wikiV result =
  template
    [(WikiR "","Wiki")]
    (\_ ->
       container
         (row
            (span12
               (case result of
                  Left err ->
                    do h1 [] "Wiki page retrieval problem!"
                       p [] (toHtml err)
                  Right (title,html) ->
                    do h1 [] (toHtml title)
                       html))))
