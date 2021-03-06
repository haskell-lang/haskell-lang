{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Documentation page view.

module HL.View.Documentation where

import Data.Map (Map)
import qualified Data.Map as Map
import HL.Types
import HL.View
import HL.View.Template

-- | Documentation view.
documentationV :: Map TutorialKey Tutorial -> View App ()
documentationV tutorialMap =
    template
        "Documentation"
        (container_
             (row_
                  (span12_
                       [class_ "col-md-12"]
                       (do h1_ "Documentation"
                           inSiteDocs tutorialMap
                           books
                           courses
                           tutorials
                           online
                           manuals
                           cabal
                           library
                           report ))))

inSiteDocs :: Map TutorialKey Tutorial -> View App ()
inSiteDocs tutorialMap = do
    h2_ "Docs on this site"
    url <- lift (asks pageRender)
    ul_ (do li_ (a_ [href_ (url GetStartedR)] "Getting started guide")
            li_ (a_ [href_ (url LibrariesR)] "Common libraries and their documentation")
            li_ (do "Locally hosted tutorials ("
                    let githubUrl = "https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/"
                    a_ [href_ githubUrl] (b_ "edit and create tutorials on Github")
                    ")"
                    ul_ (mapM_ renderTutorial (Map.toList tutorialMap))))
    p_ (do "Have other ideas for improving documentation? Please "
           a_ [href_ "https://www.reddit.com/r/haskell_lang/comments/4udlt6/documentation_priorities/"] "discuss it on Reddit")
  where
    renderTutorial :: (TutorialKey, Tutorial) -> View App ()
    renderTutorial (tutorialKey, Tutorial{tutorialTitle = title}) = do
        let route =
              case tutorialKey of
                PackageTutorial pkg -> LibraryR pkg
                RegularTutorial slug -> TutorialR slug
        url <- lift (asks pageRender)
        li_ (a_ [href_ (url route)] (toHtmlRaw title))

books :: View App ()
books =
  do h2_ "Books for Learning Haskell"
     links bookLinks
  where
    bookLinks =
      [("Haskell Programming from first principles", "http://haskellbook.com")
      ,("Haskell: The Craft of Functional Programming", "http://www.haskellcraft.com/craft3e/Home.html")
      ,("Thinking Functionally with Haskell", "http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell")
      ,("Parallel and Concurrent Programming in Haskell", "http://chimera.labs.oreilly.com/books/1230000000929")
      ,("Developing Web Applications with Haskell and Yesod", "http://www.yesodweb.com/book")]

courses :: View App ()
courses =
  do h2_ "Courses"
     p_ "Course material created by instructors"
     links courseLinks
  where
     courseLinks =
       [("University of Pennsylvania's CIS 194", "https://www.seas.upenn.edu/~cis194/")
       ,("NICTA Functional Programming Course", "https://github.com/NICTA/course")
       ,("University of Virginia's CS 1501", "http://shuklan.com/haskell/")
       ,("Stanford's cs240h", "http://www.scs.stanford.edu/14sp-cs240h/")]

tutorials :: View App ()
tutorials =
  do h2_ "Tutorials"
     p_ "Short, dense, classic ways to hit the ground running"
     links tutorialLinks
  where
    tutorialLinks =
       [("A Gentle Introduction to Haskell","https://www.haskell.org/tutorial/")
       ,("Yet Another Haskell Tutorial","http://en.wikibooks.org/wiki/Haskell/YAHT/Preamble")
       ,("Write Yourself a Scheme in 48 Hours","http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours")
       ,("Learning Haskell","http://learn.hfm.io")]

online :: View App ()
online =
  do h2_ "Online Resources"
     p_ "Curated resources put together by Haskellers:"
     links resources
  where
    resources =
      [("The Haskell Wiki", "http://wiki.haskell.org")
      ,("The Haskell Wikibook", "http://en.wikibooks.org/wiki/Haskell")
      ,("FP Complete's School of Haskell", "https://www.fpcomplete.com/school")
      ,("Stephen Diehl's What I Wish I Knew When Learning Haskell", "http://dev.stephendiehl.com/hask/#cabal")
      ,("Chris Allen's List of Learning Haskell Resources", "https://github.com/bitemyapp/learnhaskell")
      ,("Bob Ippolito's Getting Started with Haskell", "http://bob.ippoli.to/archives/2013/01/11/getting-started-with-haskell/")
      ,("Albert Y.C. Lai's Haskell Notes and Examples", "http://www.vex.net/~trebla/haskell/index.xhtml")
      ,("Learning Haskell Resources on the Haskell Wiki", "https://wiki.haskell.org/Learning_Haskell")]

manuals :: View App ()
manuals = do h2_ "Manuals and Guides"
             p_ "Manuals and guides that cover common Haskell tooling:"
             links tools
  where tools = [("GHC User Guide", "http://www.haskell.org/ghc/docs/latest/html/users_guide/")
                ,("Cabal Homepage And Quick Links", "https://www.haskell.org/cabal/")
                ,("Cabal User Guide", "http://www.haskell.org/cabal/users-guide/")
                ,("Stack User Guide", "https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md")
                ,("Haddock User Guide", "http://www.haskell.org/haddock/doc/html/index.html")
                ,("Haskeleton: A Haskell Project Skeleton", "http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/")
                ,("How to Write a Haskell Program", "https://wiki.haskell.org/How_to_write_a_Haskell_program")]

cabal :: View App ()
cabal =
  do h2_ "Package and Dependency Management"
     p_ "The Cabal guide is a good start but there's a lot to learn:"
     links cabalInfo
  where cabalInfo =
           [("Stephen Diehl's Cabal Quickstart", "http://dev.stephendiehl.com/hask/#cabal")
           ,("An Introduction to Cabal Sandboxes", "http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html")
           ,("The Storage and Interpretation of Cabal Packages", "http://www.vex.net/~trebla/haskell/sicp.xhtml")
           ,("The Cabal of Cabal", "http://www.vex.net/~trebla/haskell/cabal-cabal.xhtml")]

library :: View App ()
library =
  do h2_ "Library Documentation"
     p_ "Documentation for Haskell libraries is typically available on Hackage. We also have specialized tools for searching across it, not only by name, but by type."
     links docs
  where docs =
         [("Hoogle API Search", "http://www.haskell.org/hoogle/")
         ,("FPComplete API Search", "https://www.fpcomplete.com/hoogle")
         ,("Hayoo! API Search", "http://hayoo.fh-wedel.de")
         ,("Hackage","http://hackage.haskell.org/")
         ,("The Typeclassopedia", "https://wiki.haskell.org/Typeclassopedia")
         ,("Haddocks for Libraries included with GHC", "https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html")]

report :: View App ()
report =
  do h2_ "Language Report"
     p_ (do "The Haskell 2010 language report is available online "
            a_ [href_ "//haskell.org/onlinereport/haskell2010/"] "here"
            ". ")
     p_ (do "A PDF version is available "
            a_ [href_ "//haskell.org/definition/haskell2010.pdf"] "here"
            ".")
     p_ "It can also be downloaded as a darcs repository: "
     p_ (pre_ (code_ "$ darcs get http://darcs.haskell.org/haskell2010-report"))

links :: [(Text,Text)] -> View App ()
links items =
  ul_ (forM_ items
             (\(title,url) ->
                li_ (a_ [href_ url] (toHtml title))))
