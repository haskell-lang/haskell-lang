{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    , getPackageR
    ) where

import qualified Data.Vector as V
import           HL.Controller
import           HL.View
import           HL.View.Package
import           HL.View.Packages

-- | Packages controller.
getPackagesR :: C (Html ())
getPackagesR =
  do info <- fmap appPackageInfo getYesod
     lucid (packagesV info)

-- | Package controller.
getPackageR :: PackageName -> C (Html ())
getPackageR name =
  do info <- fmap appPackageInfo getYesod
     case V.find ((==name).packageName) (piFundamentals info) of
       Nothing -> redirect PackagesR
       Just package -> case packagePage package of
                         Nothing -> redirect PackagesR
                         Just md -> lucid (packageV name md)
