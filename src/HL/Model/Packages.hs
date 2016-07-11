{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages where

import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import           HL.Foundation
import           HL.Types
import           Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import           System.Directory

-- | Get the package info from the config.
getPackageInfo :: IO (PackageInfo Markdown)
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
                  do mmd <- getPackageMarkdown (packageName pkg) (packagePage pkg)
                     return pkg {packagePage = mmd})

-- | Get the markdown for a given package name's article.
getPackageMarkdown :: PackageName -> Maybe T.Text -> IO (Maybe Markdown)
getPackageMarkdown _ (Just url) =
  do req <- parseRequest (T.unpack url)
     res <- httpLBS req
     let lbs = getResponseBody res
         text = stripHeader (decodeUtf8With lenientDecode (L.toStrict lbs))
     return (Just (Markdown text))
getPackageMarkdown pkg Nothing =
  do exists <- doesFileExist fp
     if exists
        then do md <- T.readFile fp
                return (Just (Markdown md))
        else return Nothing
  where fp = "static/markdown/package-" ++ T.unpack (toSlug pkg) ++ ".md"

-- | If present, strip a --- header from a Markdown file.
stripHeader :: T.Text -> T.Text
stripHeader t0 = fromMaybe t0 $ do
    t1 <- T.stripPrefix "---\n" t0
    let ls = T.lines t1
    case dropWhile (/= "---") ls of
        [] -> Nothing
        (_:rest) -> Just (T.unlines rest)
