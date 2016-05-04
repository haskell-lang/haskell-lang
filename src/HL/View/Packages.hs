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
          h2_ [id_ "quicklinks"] "Quick Links"
          ul_ [class_ "quicklinks"] (mapM_ quickLink (map fundName (piFundamentals pi)))
          h2_ [id_ "fundamentals"] "Fundamentals"
          toHtml (piFundamentalsIntro pi)
          row_ (mapM_ fundamental (piFundamentals pi))
          ))))

quickLink :: Text -> Html ()
quickLink name = li_ (a_ [href_ ("#" <> name)] (toHtml name))

fundamental :: Fundamental -> Html ()
fundamental f = (span4_ [class_ "col-md-4"] (do
  h3_ [id_ (fundName f)] (toHtml (fundName f))
  toHtml (fundDesc f)
  ul_ [class_ "quicklinks"]
    (mapM_ link
       [ ("API docs", "https://www.stackage.org/package/" <> fundName f)
       ])))
  where
    link :: (Text, Text) -> Html ()
    link (title, url) = li_ (a_ [href_ url] (toHtml title))
