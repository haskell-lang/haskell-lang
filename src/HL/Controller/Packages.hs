{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    , getPackageR
    ) where

import HL.Controller
import HL.Model.Packages
import HL.View
import HL.View.Packages
import HL.View.Package

-- | Packages controller.
getPackagesR :: C (Html ())
getPackagesR =
  do info <- fmap appPackageInfo getYesod
     lucid (packagesV info)

-- | Package controller.
getPackageR :: PackageName -> C (Html ())
getPackageR name =
  do result <- getPackageMarkdown name
     case result of
       Nothing -> redirect PackagesR
       Just md -> lucid (packageV name md)
