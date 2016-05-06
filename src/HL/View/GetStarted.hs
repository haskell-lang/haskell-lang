{-# LANGUAGE OverloadedStrings #-}

-- | This page serves as basically the first interaction point for
-- users with getting hold of Haskell. Let's make it a good one.

module HL.View.GetStarted where

import HL.Types
import HL.View
import HL.View.Template

-- | Page entry point.
getStarted :: FromLucid App
getStarted =
  template []
           "Get Started with Haskell"
           (\_ -> container_ (row_ (span12_ [class_ "col-md-12"] content)))

-- | Page content.
content :: Html ()
content =
  do h1_ (toHtml ("Get Started" :: String))
     p_ "Quick steps to get up and running with Haskell quickly!"
     downloadStack

-- | The Stack download section.
downloadStack :: Html ()
downloadStack =
  do h2_ (do span_ [class_ "counter"] "1"
             " Download Haskell Stack")
     container_
       (do row_ (do span6_ [class_ "col-md-6"] operatingSystems
                    span6_ [class_ "col-md-6"] downloadContents))

-- | Operating system choices.
--
-- The user clicks on a logo and then is given a little guide on how
-- to install for that operating system.
operatingSystems :: Html ()
operatingSystems =
  do p_ [class_ "os-logos"]
        (do a_ [class_ "os-logo"
               ,href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade/#mac-os-x"]
               (img_ [src_ "/static/img/apple-logo.svg"])
            a_ [class_ "os-logo"
               ,href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows"]
               (img_ [src_ "/static/img/windows-logo.svg"])
            a_ [class_ "os-logo"
               ,href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade/#linux"]
               (img_ [src_ "/static/img/linux-logo.svg"]))
     p_ (a_ [href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade"]
            "Other operating systems")

-- | List what's inside the download.
downloadContents :: Html ()
downloadContents =
  do p_ "What's inside:"
     ul_ (do li_ (do (strong_ "Stack")
                     ": A project builder for multi-package Haskell projects.")
             li_ (do (strong_ "GHC")
                     ": A compiler and interpreter for Haskell programs.")
             li_ (do (strong_ "Haddock")
                     ": A documentation generator for Haskell packages.")
             li_ (do (strong_ "Hoogle")
                     ": A search tool for searching Haskell packages.")
             li_ "And thousands of packages installed on demand.")

-- ## span_ [class_"counter"]2/span_ Running Haskell scripts

-- To quickly run a Haskell script:

-- 1.  Copy the following content into a file called `HelloWorld.hs`:

--     ```haskell
--     #!/usr/bin/env stack
--     -- stack --install-ghc runghc

--     main :: IO ()
--     main = putStrLn "Hello World"
--     ```

-- 2. Open up a terminal and run `stack HelloWorld.hs`.

-- Done!

-- ## span_ [class_"counter"]3/span_ Write your own Haskell package

-- This is a good way to start on a proper Haskell package.

-- ```
-- stack new new-project
-- cd new-project
-- stack build
-- stack exec new-project-exe
-- ```

-- You can now edit the source files in this directory (see the file
-- `src/Lib.hs`), and run the project with `stack exec new-project-exe`
-- as above.

-- ## span_ [class_"counter"]4/span_ Next steps

-- Congratulations, you're ready to start writing Haskell code! Now you're ready to:

-- * [Learn about Haskell the language](/documentation)
-- * [Write Haskell projects with Stack](http://docs.haskellstack.org/en/stable/GUIDE/)
-- * [Browse packages that you can use in your projects](/packages)
