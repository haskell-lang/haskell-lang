{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    , getPackageR
    , getLibrariesR
    , getLibraryR
    ) where

import           HL.Controller
import           HL.Controller.Tutorial (displayTutorial)
import           HL.View
import           HL.View.Packages

----- BEGIN Hysterical raisins

getPackagesR :: C ()
getPackagesR = redirect LibrariesR

getPackageR :: PackageName -> C ()
getPackageR = redirect . LibraryR

----- END   Hysterical raisins

-- | Packages controller.
getLibrariesR :: C (Html ())
getLibrariesR =
  do info <- fmap appPackageInfo getYesod
     lucid (packagesV info)

-- | Package controller.
getLibraryR :: PackageName -> C (Html ())
getLibraryR = displayTutorial . PackageTutorial
