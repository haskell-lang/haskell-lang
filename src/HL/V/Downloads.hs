{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.V.Downloads where

import Data.Monoid
import HL.Types
import HL.V
import HL.V.Template

-- | Downloads view.
downloadsV :: FromLucid App
downloadsV =
  template [] "Downloads"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Downloads"
                   h2_ "Compiler and base libraries"
                   p_ "Downloads are available on a per operating system basis:"
                   ul_ (forM_ [minBound .. maxBound]
                              (\os ->
                                 li_ (a_ [href_ (url (DownloadsForR os))]
                                         (toHtml (toHuman os)))))
                   thirdParty))))

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

thirdParty =
  do h2_ "Third party libraries"
     p_ (do "In Haskell, packages are managed with the Cabal package system built into GHC (and other compilers). "
            "For more specific details, see "
            (a_ [href_ "https://www.haskell.org/cabal/users-guide/"] "The Cabal User Guide")
            ".")
     hackage
     stackage
     github

hackage =
  do h3_ "Hackage"
     p_ (do "Hackage is a repository of packages to which anyone can freely \
            \upload at any time. The packages are available immediately and \
            \documentation will be generated and hosted there. It can be used by "
            code_ "cabal install"
            ".")
     p_ (do "In your "
            code_ ".cabal/config"
            " file, there will by default be a line like the following:")
     pre_ "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive"
     p_ "You can install a package by merely running: "
     pre_ "$ cabal update \n\
          \$ cabal install the-package"
     p_ (a_ [href_ "https://hackage.haskell.org/packages/"] $ "Go to Hackage →")

stackage =
  do h3_ "Stackage"
     p_ "Stackage is a stable repository of snapshots of package sets in \
        \which only packages which build and pass tests together are bundled \
        \together into a snapshot."
     p_ (do "To use, in your "
            code_ ".cabal/config"
            ".cabal/config file, you specify something like the following:")
     pre_ "remote-repo: stackage:http://www.stackage.org/stackage/<the-snapshot-hash>"
     p_ "(Note: any other remote-repo line should be removed or commented out)."
     p_ "After that, you can install a package into a sandbox by merely running: "
     pre_ "$ cabal update \n\
          \$ mkdir my-project\n\
          \$ cd my-project\n\
          \$ cabal sandbox init\n\
          \$ cabal install the-package"
     p_ (a_ [href_ "http://www.stackage.org/"] $ "Go to Stackage →")

github =
  do h3_ "From source control repositories"
     p_ "Installing from a source repository is also possible. For example, \
        \to clone and install the network package from source, you would run:"
     pre_ "$ git clone git@github.com:haskell/network.git\n\
          \$ cabal install network/"
     p_ "Or:"
     pre_ "$ git clone git@github.com:haskell/network.git\n\
          \$ cd network\n\
          \$ cabal install"
     p_ (a_ [href_ "https://github.com/trending?l=haskell&since=monthly"] $
            "Browse Github by Haskell repositories →")
