{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages where

import           Control.Exception (throwIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import           HL.Foundation
import           HL.Types
import           System.Directory

-- | Get the package info from the config.
getPackageInfo :: IO PackageInfo
getPackageInfo =
  do info <-
       Yaml.decodeFileEither "config/package-info.yaml" >>=
       either throwIO return
     fundamentals <- addPages (piFundamentals info)
     commons <-
       mapM (\c ->
               do choices <- addPages (commonChoices c)
                  return c {commonChoices = choices})
            (piCommons info)
     return (info {piFundamentals = fundamentals
                  ,piCommons = commons})
  where addPages =
          mapM (\pkg ->
                  do mmd <- getPackageMarkdown (packageName pkg)
                     return pkg {packagePage = mmd})

-- | Get the markdown for a given package name's article.
getPackageMarkdown :: PackageName -> IO (Maybe Markdown)
getPackageMarkdown pkg =
  do exists <- doesFileExist fp
     if exists
        then do md <- T.readFile fp
                return (Just (Markdown md))
        else return Nothing
  where fp = "static/markdown/package-" ++ T.unpack (toSlug pkg) ++ ".md"
