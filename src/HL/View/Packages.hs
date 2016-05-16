{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Packages page view.

module HL.View.Packages
    ( packagesV
    ) where

import Data.Foldable
import Data.List.Split
import Data.Monoid ((<>))
import HL.Types
import HL.View
import HL.View.Template
import Prelude hiding (pi)

-- | Packages view.
packagesV :: PackageInfo -> FromLucid App
packagesV pi =
  template []
           "Packages"
           (\url ->
              (container_
                 (row_ (span12_ [class_ "col-md-12"]
                                (content url pi)))))

content :: (Route App -> Text) -> PackageInfo -> Html ()
content url pi =
  do h1_ (toHtml ("Haskell Packages" :: String))
     toHtml (piIntro pi)
     h2_ [id_ "core"] "Core"
     toHtml (piFundamentalsIntro pi)
     mapM_ (row_ . mapM_ (span3_ [class_ "col-md-3"] . package url False))
           (chunksOf 4 (toList (piFundamentals pi)))
     h2_ [id_ "common"] "Common"
     toHtml (piCommonsIntro pi)
     mapM_ (row_ . mapM_ (common url))
           (chunksOf 2 (toList (piCommons pi)))

package :: (Route App -> Text) -> Bool -> Package -> Html ()
package url isCommon f =
  a_ [class_ "package-big-link"
     ,href_ ref]
     (do let heading_ =
               if isCommon
                  then h4_
                  else h3_
         heading_ [id_ (toSlug (packageName f))]
                  (toHtml (packageName f))
         span_ [class_ "pkg-desc"]
               (toHtml (packageDesc f)))
  where ref = if packageTutorial f
                 then url (PackageR (packageName f))
                 else ("https://www.stackage.org/package/" <> toSlug (packageName f))

common :: (Route App -> Text) -> Common -> Html ()
common url c =
  span6_ [class_ "col-md-6"]
         (do do let ident = "_common_" <> commonSlug c
                h3_ [id_ ident]
                    (a_ [href_ ("#" <> ident)]
                        (toHtml (commonTitle c)))
                toHtml (commonDesc c)
                row_ (mapM_ (span6_ [class_ "col-md-6"] . package url True)
                            (commonChoices c)))
