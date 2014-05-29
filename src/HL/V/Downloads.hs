{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Downloads page view.

module HL.V.Downloads where

import Data.Monoid
import HL.Types
import HL.V hiding (list)
import HL.V.Template

-- | Downloads view.
downloadsV :: FromSenza App
downloadsV =
  template
    []
    "Downloads"
    (\url ->
       container
         (row
            (span12
               []
               (do h1 [] "Downloads"
                   h2 [] "Compiler and base libraries"
                   p [] "Downloads are available on a per operating system basis:"
                   ul []
                      (forM_ [minBound .. maxBound]
                             (\os -> li [] (a [href (url (DownloadsForR os))]
                                              (toHtml (toHuman os)))))
                   h2 [] "Third party libraries"
                   p [class_ "muted"]
                     "Explanation of Hackage and Stackage here."))))

-- | OS-specific downloads view.
downloadsForV :: OS -> FromSenza App
downloadsForV os =
  template
    [DownloadsR
    ,DownloadsForR os]
    ("Downloads for " <> toHuman os)
    (\_ ->
       container
         (row
            (span12
               []
               (do h1 [] (toHtml ("Downloads for " <> toHuman os))
                   p [] "Coming soon."))))
