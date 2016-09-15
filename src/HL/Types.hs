{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Side-wide datatypes.

module HL.Types where

import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Aeson
import           Data.Char
import           Data.Map (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Typeable
import           Data.Vector (Vector)
import           Language.Haskell.HsColour.CSS (hscolour)
import           Lucid
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Blaze.Html5 as Blaze (pre)
import qualified Text.Markdown as MD
import           Yesod.Core (HandlerT, WidgetT, Html)
import           Yesod.Core.Dispatch
import           Yesod.Feed
import           Yesod.GitRev (GitRev)
import           Yesod.Slug
import           Yesod.Static

-- | Make a human-readable version of the value.
class Human a where
  toHuman :: a -> Text

-- | A haskell-lang exception.
data HaskellLangException
  = MarkdownFileUnavailable !FilePath
  | ReportPageNotFound !FilePath
  deriving (Show, Typeable, Eq)

instance Exception HaskellLangException

type URL = Text
type AnnouncementID = Text

-- | Application state.
data App = App
  { appStatic        :: !Static
  , appCacheDir      :: !(MVar FilePath)
  , appPackageInfo   :: !PackageInfo
  , appDefaultLayout :: !(WidgetT App IO () -> HandlerT App IO Yesod.Core.Html)
  , appFeedEntries   :: ![(FeedEntry (Maybe URL), AnnouncementID)]
  , appGitRev        :: !GitRev
  , appSnippetInfo   :: !SnippetInfo
  , appTutorials     :: !(Map TutorialKey Tutorial)
  }

data PackageInfo = PackageInfo
    { piIntro :: !Markdown
    , piFundamentalsIntro :: !Markdown
    , piFundamentals :: !(Vector Package)
    , piCommonsIntro :: !Markdown
    , piCommons :: !(Vector Common)
    } deriving (Show)
instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> o .: "intro"
        <*> o .: "fundamentals-intro"
        <*> o .: "fundamentals"
        <*> o .: "commons-intro"
        <*> o .: "commons"

data Package = Package
    { packageName :: !PackageName
    , packageDesc :: !Markdown
    } deriving (Show)

instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .: "name"
        <*> o .: "description"

data Common = Common
    { commonTitle :: !Text
    , commonSlug :: !Text
    , commonDesc :: !Markdown
    , commonChoices :: !(Vector Package)
    } deriving (Show)
instance FromJSON Common where
    parseJSON = withObject "Common" $ \o -> Common
        <$> o .: "title"
        <*> o .: "slug"
        <*> o .: "description"
        <*> o .: "choices"

data TutorialKey
    = RegularTutorial !Text
    | PackageTutorial !PackageName
    deriving (Show, Eq, Ord)

data Tutorial = Tutorial
    { tutorialTitle :: !Text
    , tutorialContent :: !Markdown
    , tutorialLocalFilename :: !(Maybe Text)
    }
    deriving (Show)

newtype Markdown = Markdown { unMarkdown :: Text }
  deriving (FromJSON,Show)
instance ToHtml Markdown where
  toHtml = toHtmlRaw
  toHtmlRaw =
    toHtmlRaw .
    Blaze.renderHtml . MD.markdown settings . TL.fromStrict . unMarkdown
    where settings =
            MD.def {MD.msXssProtect = False
                   ,MD.msBlockCodeRenderer =
                      \mlang (unrendered,rendered) ->
                        case mlang of
                          Just "haskell" ->
                            Blaze.preEscapedString (hscolour False 1 (T.unpack unrendered))
                          _ -> Blaze.pre rendered}

newtype PackageName = PackageName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, ToHtml)

instance Read PackageName where
  readsPrec _ str =
    if T.all (\c -> c =='-' || c == '_' || isAlphaNum c) (T.pack str)
       then [(PackageName (T.pack str),"")]
       else []

instance Slug PackageName where
  toSlug (PackageName name) = name

instance Human PackageName where
  toHuman (PackageName name) = name

instance PathPiece PackageName where
  toPathPiece = toSlug
  fromPathPiece t =
    if T.all (\c -> c =='-' || c == '_' || isAlphaNum c) t
       then Just (PackageName t)
       else Nothing

-- | Operating system. Used for downloads, for example.
data OS = Windows | OSX | Linux
  deriving (Read,Show,Typeable,Eq,Enum,Bounded)

instance Slug OS where
  toSlug o =
    case o of
      Windows -> "windows"
      OSX     -> "osx"
      Linux   -> "linux"

instance Human OS where
  toHuman o =
    case o of
      Windows -> "Windows"
      OSX     -> "OS X"
      Linux   -> "Linux"

instance PathPiece OS where
  toPathPiece = toSlug
  fromPathPiece t =
    case t of
      "osx"     -> Just OSX
      "windows" -> Just Windows
      "linux"   -> Just Linux
      _         -> Nothing

-- | Mode for rendering Haskell report.
data Mode = Mono | Node
  deriving (Eq, Show, Read)

instance Slug Mode where
  toSlug m =
    case m of
      Mono -> "mono"
      Node -> "node"

instance PathPiece Mode where
  fromPathPiece m =
    case m of
      "mono" -> Just Mono
      "node" -> Just Node
      _ -> Nothing
  toPathPiece = toSlug

-- | Page cache key.
data PageKey = Report !Int

instance Slug PageKey where
  toSlug (Report year) = "report-" <> T.pack (show year)

data SnippetInfo =
  SnippetInfo {siSnippets :: [Snippet]
              ,siSeed :: !Int}
  deriving (Show)

instance FromJSON SnippetInfo where
  parseJSON j =
    do o <- parseJSON j
       snippets <- o .: "snippets"
       return (SnippetInfo snippets 1)

data Snippet =
  Snippet {snippetTitle :: !Text
          ,snippetCode :: !Text}
  deriving (Show)


instance FromJSON Snippet where
  parseJSON j =
    do o <- parseJSON j
       title <- o .: "title"
       code <- o .: "code"
       return (Snippet title code)
