{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    , getPackageR
    ) where

import           Data.Monoid ((<>))
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
     case V.find ((== name) . packageName)
                 (piFundamentals info) of
       Nothing ->
         case V.find ((== name) . packageName)
                     (V.concatMap commonChoices
                                  (piCommons info)) of
           Nothing -> onNotFound
           Just package -> handlePackage package
       Just package -> handlePackage package
  where handlePackage package =
          case packagePage package of
            Nothing -> onNotFound
            Just md -> lucid (packageV name md)

        onNotFound = redirect $ "https://www.stackage.org/package/" <> toPathPiece name
