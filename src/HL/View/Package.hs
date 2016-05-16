{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Packages page view.

module HL.View.Package
    ( packageV
    ) where

import Data.Monoid ((<>))
import HL.Types
import HL.View
import HL.View.Template
import Prelude hiding (pi)

-- | Packages view.
packageV :: PackageName -> Markdown -> FromLucid App
packageV pkgname md =
  template []
           ("Package: " <> toHuman pkgname)
           (\_ ->
              (container_
                 (do row_ (span12_ [class_ "col-md-12"]
                                   (h1_ (toHtml ("Package: " <> toHuman pkgname))))
                     row_ (span12_ [class_ "col-md-12"]
                                   (toHtml md)))))
