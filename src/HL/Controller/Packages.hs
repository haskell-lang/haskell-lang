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

import qualified Data.Vector as V
import           HL.Controller
import           HL.View
import           HL.View.Package
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
getLibraryR name =
  do info <- fmap appPackageInfo getYesod
     case V.find ((== name) . packageName)
                 (piFundamentals info) of
       Nothing ->
         case V.find ((== name) . packageName)
                     (V.concatMap commonChoices
                                  (piCommons info)) of
           Nothing -> redirect PackagesR
           Just package -> handlePackage package
       Just package -> handlePackage package
  where handlePackage package =
          case packagePage package of
            Nothing -> redirect PackagesR
            Just md -> lucid (packageV name md)
