{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Home page view.

module HL.V.Home where

import HL.Foundation
import HL.V.Template
import Blaze.Elements as E
import Blaze.Prelude
import Blaze.Bootstrap

-- | Home view.
homeV :: Blaze App
homeV =
  template
    (\url ->
       container
         (row
            (span12
               (do h1 "hi"
                   with a [class_ "btn btn-primary"] "test"))))
