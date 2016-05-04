{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Lucid
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Markdown as MD

newtype Markdown = Markdown { unMarkdown :: Text }
  deriving FromJSON
instance ToHtml Markdown where
  toHtml = toHtmlRaw
  toHtmlRaw =
      toHtmlRaw . Blaze.renderHtml . MD.markdown settings . TL.fromStrict . unMarkdown
    where
      settings = MD.def
          { MD.msXssProtect = False
          }

data PackageInfo = PackageInfo
    { piIntro :: !Markdown
    , piFundamentalsIntro :: !Markdown
    , piFundamentals :: ![Fundamental]
    }
instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> o .: "intro"
        <*> o .: "fundamentals-intro"
        <*> o .: "fundamentals"

data Fundamental = Fundamental
    { fundName :: !Text
    , fundDesc :: !Markdown
    -- TODO once we have markdown files for tutorials, we can include
    -- a Bool here indicating whether a tutorial exists
    }

instance FromJSON Fundamental where
    parseJSON = withObject "Fundamental" $ \o -> Fundamental
        <$> o .: "name"
        <*> o .: "description"
