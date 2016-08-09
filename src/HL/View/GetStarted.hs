{-# LANGUAGE OverloadedStrings #-}

-- | This page serves as basically the first interaction point for
-- users with getting hold of Haskell. Let's make it a good one.

module HL.View.GetStarted where

import HL.Types
import HL.View
import HL.View.Code
import HL.View.Template

-- | Page entry point.
getStarted :: Maybe OS -> View App ()
getStarted mos =
  template
           "Get Started with Haskell"
           (do url <- lift (asks pageRender)
               div_ [class_ "get-started-general"]
                    (content url mos))

-- | Page content.
content :: (Route App -> Text) -> Maybe OS -> View App ()
content url mos =
  do container_
       (row_ (span12_ [class_ "col-md-12"]
                      (do h1_ (toHtml ("Get Started" :: String))
                          p_ "Quick steps to get up and running with Haskell.")))
     downloadStack url mos
     container_
       (do h2_ (do span_ [class_ "counter"] "2 "
                   "Running Haskell programs")
           row_ (do span6_ [class_ "col-md-6"] runScripts
                    span6_ [class_ "col-md-6"] writePackage))
     container_
       (row_ (span12_ [class_ "col-md-12"]
                      (nextSteps url)))

-- | The Stack download section.
downloadStack :: (Route App -> Text) -> Maybe OS -> View App ()
downloadStack url mos =
  do container_
       (do row_ (do span12_ [class_ "col-md-12"]
                            (h2_ (do span_ [class_ "counter"] "1 "
                                     "Download Haskell Stack")))
           row_ (do span6_ [class_ "col-md-6"]
                           (do operatingSystems url mos
                               operatingSystemDownload url mos)
                    span6_ [class_ "col-md-6"] downloadContents))

-- | Operating system choices.
--
-- The user clicks on a logo and then is given a little guide on how
-- to install for that operating system.
operatingSystems :: (Route App -> Text) -> Maybe OS -> View App ()
operatingSystems url mos =
  do case mos of
       Nothing -> p_ "Choose your operating system:"
       Just os -> p_ (do "Chosen operating system: "
                         strong_ (toHtml (toHuman os)))
     p_ [class_ "os-logos"]
        (do forM_ oses
                  (\(os,osLogo) ->
                     a_ (concat [[class_ "os-logo"
                                 ,href_ (url (GetStartedOSR os))
                                 ,title_ (toHuman os)]
                                ,case mos of
                                   Nothing -> [class_ " os-choose "]
                                   Just os'
                                     | os' == os -> [class_ " os-selected "]
                                     | otherwise -> [class_ " os-faded "]])
                        (img_ [src_ (url (StaticR osLogo))])))
  where oses =
          [(OSX,img_apple_logo_svg)
          ,(Windows,img_windows_logo_svg)
          ,(Linux,img_linux_logo_svg)]

-- | Show download information for the operating system.
operatingSystemDownload :: (Route App -> Text) -> Maybe OS -> View App ()
operatingSystemDownload url mos =
  case mos of
    Nothing -> return ()
    Just OSX ->
      do p_ "Run the following in your terminal:"
         osxWindow "Terminal"
                   (div_ [class_ "terminal-sample"]
                         (do span_ [class_ "noselect"] "$ "
                             span_ "curl -sSL https://get.haskellstack.org/ | sh"))
         p_ (a_ [href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade/#mac-os-x"]
                "More detailed installation information")
    Just Linux ->
      linuxDownload url
    Just Windows ->
      do p_ (do "Download and run the installer: ")
         ul_ (do li_ (a_ [href_ "https://www.stackage.org/stack/windows-x86_64-installer"]
                         "Windows 64-bit")
                 li_ (a_ [href_ "https://www.stackage.org/stack/windows-i386-installer"]
                         "Windows 32-bit"))

-- | Linux download details.
linuxDownload :: (Route App -> Text) -> View App ()
linuxDownload url =
  do p_ "Run the following in your terminal:"
     osxWindow "Terminal"
               (div_ [class_ "terminal-sample"]
                     (do span_ [class_ "noselect"] "$ "
                         span_ "wget -qO- https://get.haskellstack.org/ | sh"))
     div_ [class_ "muted-choices"]
          (do p_ "Or for distribution-specific packages, choose your Linux distribution:"
              p_ (forM_ distros
                        (\(title,logo',link) ->
                           a_ [href_ link]
                              (img_ [src_ (url (StaticR logo'))
                                    ,title_ title
                                    ,class_ "distro-logo"]))))
     p_ (do "Or "
            a_ [href_ "http://docs.haskellstack.org/en/stable/install_and_upgrade/"]
               "get more detailed info, more Linux distributions, etc.")
  where distros =
          [("Ubuntu"
           ,img_ubuntu_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu")
          ,("Arch"
           ,img_arch_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#arch-linux")
          ,("Debian"
           ,img_debian_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#debian")
          ,("Fedora"
           ,img_fedora_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#fedora")
          ,("NixOS"
           ,img_nixos_logo_png
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#nixos")
          ,("OpenSUSE"
           ,img_opensuse_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#opensuse-suse-linux-enterprise")
          ,("Redhat"
           ,img_redhat_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#centos")
          ,("Centos"
           ,img_centos_logo_svg
           ,"http://docs.haskellstack.org/en/stable/install_and_upgrade/#centos")]

-- | List what's inside the download.
downloadContents :: View App ()
downloadContents =
  do p_ "With the Haskell Stack you get a comprehensive \
        \development environment for Haskell:"
     ul_ (do li_ (do (strong_ "Stack")
                     ": A project builder for multi-package Haskell projects.")
             li_ (do (strong_ "Intero")
                     ": Complete interactive development program for Haskell.")
             li_ (do (strong_ "GHC")
                     ": A compiler and interpreter for Haskell programs.")
             li_ (do (strong_ "Haddock")
                     ": A documentation generator for Haskell packages.")
             --li_ (do (strong_ "Hoogle")
             --        ": A search tool for searching Haskell packages.")
             li_ "And thousands of packages installed on demand.")

-- | Demo of running a Haskell script with Stack.
runScripts :: View App ()
runScripts =
  do p_ "To quickly run a Haskell script:"
     ol_ (do li_ (do p_ "Copy the following content into a file called `HelloWorld.hs`:"
                     haskellPre
                       "#!/usr/bin/env stack\n\
                                 \-- stack --install-ghc runghc\n\
                                 \\n\
                                 \main :: IO ()\n\
                                 \main = putStrLn \"Hello World\"")
             li_ "Open up a terminal and run `stack HelloWorld.hs`.")
     p_ "Done!"

-- | Example of making a package to be built with Stack.
writePackage :: View App ()
writePackage =
  do p_ "Start on a proper Haskell package. \
        \Run the following in your terminal:"
     osxWindow "Terminal"
               (div_ [class_ "terminal-sample"]
                     (forM_ (lines ls)
                            (\l ->
                               (do span_ [class_ "noselect"] "$ "
                                   span_ (do toHtml l
                                             "\n")))))
     p_ (do "You can now edit the source files in this directory \
            \(see the file "
            code_ "src/Lib.hs"
            "), and rebuild and run the project with "
            code_ "stack build"
            " and "
            code_ "stack exec new-project-exe"
            " as above.")
  where ls =
          "stack new new-project\n\
          \cd new-project\n\
          \stack build\n\
          \stack exec new-project-exe"

-- | Next steps for the user to go to.
nextSteps :: (Route App -> Text) -> View App ()
nextSteps url =
  do h2_ (do span_ [class_ "counter"] "3 "
             "Next steps")
     p_ "Congratulations, you're setup to start writing \
        \Haskell code! Now you're ready to:"
     ul_ (mapM_ (\(title,link) -> li_ (a_ [href_ link] title)) nextLinks)
  where nextLinks =
          [("Learn about Haskell the language",url DocumentationR)
          ,("Write Haskell projects with Stack"
           ,"http://docs.haskellstack.org/en/stable/GUIDE/")
          ,("Improve your development workflow with intero",url InteroR)
          ,("Browse libraries that you can use in your projects",url LibrariesR)]
