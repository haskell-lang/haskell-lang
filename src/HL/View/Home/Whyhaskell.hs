{-# LANGUAGE OverloadedStrings #-}

-- | Features list.

module HL.View.Home.Whyhaskell (whyhaskell) where

import HL.View

-- | Whyhaskell section explains what's notable about Haskell as a
-- language.
whyhaskell :: View App ()
whyhaskell =
  div_ [class_ "whyhaskell"]
       (container_
          (do h1_ "Why Haskell"
              row_ (do span6_ [class_ "col-md-6"] beprod
                       span6_ [class_ "col-md-6"] makeit)
              row_ (do span6_ [class_ "col-md-6"] community
                       span6_ [class_ "col-md-6"] training)
              row_ (do span6_ [class_ "col-md-6"] community2
                       span6_ [class_ "col-md-6"] rich)
              row_ (do span6_ [class_ "col-md-6"] deep
                       span6_ [class_ "col-md-6"] domains )))

beprod :: View App ()
beprod = do h2_ "Be productive, be correct"
            p_ "Haskell’s combination of strong type system, type inference, and high level code gives you the speed of development of a scripting language like Python or Ruby, yet more robustness than common statically typed languages like Java or C#. Stop finding bugs in production, let the compiler help you make more robust applications today!"

makeit :: View App ()
makeit = do h2_ "Make it work, make it fast!"
            p_ "Haskell defaults to memory safe operations on immutable values, making it easy to create working code quickly. But when you’re ready to optimize, you can drop down to explicit memory management, mutable objects, or even call out to the FFI."

community :: View App ()
community = do
    h2_ "Large, active community, plus commercial support"
    p_
        "The Haskell open source community is highly active, with strong presences in mailing lists, IRC, Stack Overflow, Reddit, and more. Pick a medium you want to converse on, and odds are you’ll find a vibrant Haskell community."
community2 =
    p_
        "In addition, the Commercial Haskell group provides a forum for commercial users to get help and collaborate on projects. Large suppliers of commercial support, like FP Complete, are available to help you succeed, as well as a large number of individual consultants."

training :: View App ()
training = do
    h2_ "High quality training materials"
    p_
        "It’s never been easier to get started with Haskell. Check out our documentation page which covers topics from the simple to advanced. And don’t forget to read Haskell Programming from First Principles."

deep :: View App ()
deep = do
    h2_ "Deep roots, modern tools"
    p_
        "Haskell first appeared in 1990, born out of deep collaboration in the academic world to create a real world functional programming language. In its 26 years of existence, Haskell has been improved and refined, and has had a significant impact on many other programming languages. At the same time, it sports modern tooling:"

    do
      p_
          "GHC, Haskell’s primary compiler, features an advanced multithreaded runtime, compacting garbage collector tuned for immutable objects, customizable optimizations (via rewrite rules), and compilation to Javascript"
      p_
          "Stack, Haskell’s build tool of choice, combines the best practices of tools from other languages, featuring automated toolchain setup, multiple toolchain versions in parallel, curated package sets, and reproducible build plans."
      p_
          "Intero provides you with intuitive, reliable editor integration. Your program is strongly typed: use that information to help you be productive!"

rich :: View App ()
rich = do
    h2_ "Rich library ecosystem"
    p_
        "Haskell features over 10,000 open source libraries to choose from. And with both LTS (long term support) Haskell and Stackage Nightly, you can choose whether you live on the stable or bleeding edge of library development."

domains :: View App ()
domains = do
    h2_ "Shines in many problem domains"
    p_
        "Haskell is used throughout industry for a wide array of problem domains, including:"

    do
      ul_
          (do li_ "Front end web development"
              li_ "Back end web development"
              li_ "Concurrent and parallel programming"
              li_ "Cluster computing"
              li_ "Financial modeling"
              li_ "Scientific and biotech modeling"
              li_ "Machine learning"
              li_ "Parsing")
