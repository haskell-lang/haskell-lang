{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    ) where

import HL.Controller
import HL.View.Packages
import HL.View

-- | Packages controller.
getPackagesR :: C (Html ())
getPackagesR = getYesod >>= lucid . packagesV . appPackageInfo
