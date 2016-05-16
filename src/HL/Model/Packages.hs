{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           HL.Foundation
import           HL.Types
import           System.Directory
import           Yesod

-- | Get the markdown for a given package name's article.
getPackageMarkdown :: PackageName -> Handler (Maybe Markdown)
getPackageMarkdown pkg =
  do exists <- liftIO (doesFileExist fp)
     if exists
        then do md <- liftIO (T.readFile fp)
                return (Just (Markdown md))
        else return Nothing
  where fp = "static/markdown/package-" ++ T.unpack (toSlug pkg) ++ ".md"
