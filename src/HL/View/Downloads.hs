{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.View.Downloads where

import Data.Monoid
import HL.Types
import HL.View
import HL.View.Template

hpSection :: Html ()
hpSection = do
  let hpRoot = "http://www.haskell.org/platform/"
  h2_ "Haskell Platform"
  p_ $ "The Haskell Platform is a convenient way to install the Haskell development tools and"
       <> " a collection of commonly used Haskell packages from Hackage."
  p_ $ "Get the Haskell Platform for:"
  ul_ $ do li_ $ a_ [href_ $ hpRoot <> "windows.html"] "Windows"
           li_ $ a_ [href_ $ hpRoot <> "mac.html"] "OS X"
           li_ $ a_ [href_ $ hpRoot <> "linux.html"] "Linux"
  hr_ [style_ "height: 1px; background-color: black;"]

-- | Downloads view.
downloadsV :: FromLucid App
downloadsV =
  template [] "Downloads"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Downloads"
                   hpSection
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

thirdParty :: Html ()
thirdParty =
  do h2_ "Third party libraries"
     p_ (do "In Haskell, packages are managed with the Cabal package system built into GHC (and other compilers). "
            "For more specific details, see "
            (a_ [href_ "https://www.haskell.org/cabal/users-guide/"] "The Cabal User Guide")
            ".")
     hackage
     ltsHaskell
     stackage
     github

hackage :: Html ()
hackage =
  do h3_ "Hackage"
     p_ (do "Hackage is a repository of packages to which anyone can freely \
            \upload at any time. The packages are available immediately and \
            \documentation will be generated and hosted there. It can be used by "
            code_ "cabal install"
            ".")
     p_ "You can install a package by merely running: "
     pre_ "$ cabal update \n\
          \$ cabal install the-package"
     p_ (a_ [href_ "https://hackage.haskell.org/packages/"] $ "Go to Hackage →")

ltsHaskell :: Html ()
ltsHaskell =
  do h3_ "LTS Haskell"
     p_ "LTS Haskell is a stackage-based long-term support set of packages \
        \which build and pass tests together, with backported bug fixes."
     p_ (a_ [href_ "http://www.stackage.org/lts"] $ "Get LTS Haskell →")

stackage :: Html ()
stackage =
  do h3_ "Stackage Nightly"
     p_ "Stackage is a nightly generated stable repository of snapshots of package sets in \
        \which only packages which build and pass tests together are bundled \
        \together into a snapshot."
     p_ (a_ [href_ "http://www.stackage.org/nightly"] $ "Get Stackage Nightly →")

github :: Html ()
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
