{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Documentation page view.

module HL.V.Documentation where

import HL.V
import HL.V.Template

-- | Documentation view.
documentationV :: FromSenza App
documentationV =
  template
    []
    "Documentation"
    (\url ->
       container
         (row
            (span12
               []
               (do h1 [] "Documentation"
                   books
                   manuals
                   report url))))

books =
  do h2 [] "Books"
     p [] "Latest books for learning Haskell:"
     links modern
     p [] "Older books:"
     links older
  where
    modern =
      [("Learn You a Haskell for Great Good!","http://learnyouahaskell.com/")
      ,("Real World Haskell","http://book.realworldhaskell.org/")
      ,("Beginning haskell","http://www.apress.com/9781430262503")]
    older =
      [("Programming in Haskell","http://www.cs.nott.ac.uk/~gmh/book.html")
      ,("Haskell: The Craft of Functional Programming","http://www.cs.ukc.ac.uk/people/staff/sjt/craft2e/")
      ,("Introduction to Functional Programming using Haskell","http://www.prenhall.com/allbooks/ptr_0134843460.html")]


manuals = do h2 [] "Manuals"
             p [] "Manuals that cover common Haskell tooling:"
             links tools
  where tools = [("GHC User Guide","http://www.haskell.org/ghc/docs/latest/html/users_guide/")
                ,("Cabal User Guide","http://www.haskell.org/cabal/users-guide/")
                ,("Haddock User Guide","http://www.haskell.org/haddock/doc/html/index.html")
                ,("What I Wish I Knew When Learning Haskell","http://dev.stephendiehl.com/hask/#cabal")]

report :: (Route App -> AttributeValue) -> Html
report url =
  do h2 [] "Language Report"
     p []
       (do "The Haskell 2010 language report is available online "
           a [href (url (ReportHomeR 2010))]
             "here"
           ". "
           todo "(But the formatting is not quite right yet.)")
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

links items =
  ul []
     (forM_ items
            (\(title,url) ->
               li [] (a [href url] title)))
