{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | News page view.

module HL.V.News where

import HL.V
import HL.V.Template

-- | News view.
newsV :: Html () -> FromLucid App
newsV inner =
  template []
           "News"
           (\_ ->
              container_
                (do row_ (span12_ [class_ "col-md-12"] (do h1_ "News"))
                    inner))
