{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.View.Downloads where

import Data.Monoid
import HL.Types
import HL.View
import HL.View.Template

downloadsFromMarkdown :: Html () -> FromLucid App
downloadsFromMarkdown md =
  template [] "Downloads"
    (\_ -> container_ (row_  (span12_ [class_ "col-md-12"]
                                      (do h1_ (toHtml ("Downloads" :: String))
                                          md))))

-- | OS-specific downloads view.
downloadsForV :: OS -> Html () -> Html () -> FromLucid App
downloadsForV os autoInstall manualInstall =
  template
    [DownloadsR
    ,DownloadsForR os]
    ("Downloads for " <> toHuman os)
    (\_ ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ (toHtml ("Downloads for " <> toHuman os))
                   autoInstall
                   when (os == Linux)
                        (do h2_ "Manual install"
                            p_ "To install GHC and Cabal manually, follow these steps."
                            manualInstall)))))
