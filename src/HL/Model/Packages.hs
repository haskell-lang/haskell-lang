{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages (getPackageInfo) where

import           Control.Exception (throwIO)
import qualified Data.Yaml as Yaml
import           HL.Types

-- | Get the package info from the config.
getPackageInfo :: IO PackageInfo
getPackageInfo =
       Yaml.decodeFileEither "config/package-info.yaml" >>=
       either throwIO return
