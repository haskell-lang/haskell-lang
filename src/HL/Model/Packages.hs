{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Packages where

import Data.Aeson
import Data.Text (Text)
import Data.Vector (Vector)
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
    , piFundamentals :: !(Vector Package)
    , piCommonsIntro :: !Markdown
    , piCommons :: !(Vector Common)
    }
instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> o .: "intro"
        <*> o .: "fundamentals-intro"
        <*> o .: "fundamentals"
        <*> o .: "commons-intro"
        <*> o .: "commons"

data Package = Package
    { packageName :: !Text
    , packageDesc :: !Markdown
    -- TODO once we have markdown files for tutorials, we can include
    -- a Bool here indicating whether a tutorial exists
    }

instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .: "name"
        <*> o .: "description"

data Common = Common
    { commonTitle :: !Text
    , commonSlug :: !Text
    , commonDesc :: !Markdown
    , commonChoices :: !(Vector Package)
    }
instance FromJSON Common where
    parseJSON = withObject "Common" $ \o -> Common
        <$> o .: "title"
        <*> o .: "slug"
        <*> o .: "description"
        <*> o .: "choices"
