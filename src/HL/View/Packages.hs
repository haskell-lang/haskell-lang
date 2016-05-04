{-# LANGUAGE OverloadedStrings #-}

-- | Packages page view.

module HL.View.Packages
    ( packagesV
    ) where

import Data.Monoid ((<>))
import HL.Model.Packages
import HL.View
import HL.View.Template
import Prelude hiding (pi)

-- | Packages view.
packagesV :: PackageInfo -> FromLucid App
packagesV pi = template [] "Packages"
    (\_ -> container_ (row_  (span12_ [class_ "col-md-12"]
      (do h1_ (toHtml ("Haskell Packages" :: String))
          toHtml (piIntro pi)
          h2_ [id_ "_quicklinks"] "Quick Links"
          ul_ [class_ "quicklinks"]
            (do mapM_ quickLink (fmap packageName (piFundamentals pi))
                mapM_ quickLink (fmap packageName (foldMap commonChoices (piCommons pi))))
          h2_ [id_ "_fundamentals"] "Fundamentals"
          toHtml (piFundamentalsIntro pi)
          row_ (mapM_ (package False) (piFundamentals pi))
          h2_ [id_ "_commons"] "Commons"
          toHtml (piCommonsIntro pi)
          mapM_ common (piCommons pi)
          ))))

quickLink :: Text -> Html ()
quickLink name = li_ (a_ [href_ ("#" <> name)] (toHtml name))

package :: Bool -> Package -> Html ()
package isCommon f = (span4_ [class_ "col-md-4"] (do
  let heading_ = if isCommon then h4_ else h3_
  heading_ [id_ (packageName f)]
    (a_ [href_ ("#" <> packageName f)] (toHtml (packageName f)))
  toHtml (packageDesc f)
  ul_ [class_ "quicklinks"]
    (mapM_ link
       [ ("API docs", "https://www.stackage.org/package/" <> packageName f)
       ])))
  where
    link :: (Text, Text) -> Html ()
    link (title, url) = li_ (a_ [href_ url] (toHtml title))

common :: Common -> Html ()
common c = do
    let ident = "_common_" <> commonSlug c
    h3_ [id_ ident] (a_ [href_ ("#" <> ident)] (toHtml (commonTitle c)))
    toHtml (commonDesc c)
    row_ (mapM_ (package True) (commonChoices c))
