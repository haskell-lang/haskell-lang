{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Documentation page view.

module HL.View.Documentation where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Documentation view.
documentationV :: Blaze App
documentationV =
  template
    [(DocumentationR,"Documentation")]
    (\url ->
       container
         (row
            (span12
               (do h1 [] "Documentation"
                   online url
                   report url))))

online url =
  do h2 [] "Online Resources"
     p [] "There are various online resources for learning Haskell; books, \
          \articles, videos, etc. below are some of the highlights:"


report url =
  do h2 [] "Language Report"
     p []
       (do "The Haskell 2010 language report is available online "
           a [href (url (ReportR 2010))]
             "here"
           ".")
     p []
       (do "A PDF version is available "
           a [href "http://haskell.org/definition/haskell2010.pdf"]
             "here"
           ".")
     p []
       "It can also be downloaded as a darcs repository: "
     p []
       (pre []
            (code []
                  "$ darcs get http://darcs.haskell.org/haskell2010-report"))
