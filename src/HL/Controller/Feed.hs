{-# LANGUAGE OverloadedStrings #-}
module HL.Controller.Feed
    ( getFeedR
    , FeedEntryWrapper (..)
    ) where

import Data.Aeson
import Data.Time (getCurrentTime)
import HL.Controller
import Text.Markdown (markdown, def, msXssProtect)
import Yesod.Feed


getFeedR :: C TypedContent
getFeedR = do
    now <- liftIO getCurrentTime
    render <- getUrlRender
    entries' <- fmap appFeedEntries getYesod
    let entries = filter ((<= now) . feedEntryUpdated) entries'
        updated =
            case entries of
                entry:_ -> feedEntryUpdated entry
                [] -> now
    newsFeedText Feed
        { feedTitle = "Haskell Language News Feed"
        , feedLinkSelf = render FeedR
        , feedLinkHome = render HomeR
        , feedAuthor = "haskell-lang.org team"
        , feedDescription = "Announcements about the Haskell programming language"
        , feedUpdated = updated
        , feedLanguage = "en_US"
        , feedLogo = Nothing
        , feedEntries = entries
        }

newtype FeedEntryWrapper = FeedEntryWrapper { toFeedEntry :: FeedEntry Text }
instance FromJSON FeedEntryWrapper where
    parseJSON = withObject "FeedEntryWrapper" $ \o ->
        fmap FeedEntryWrapper $ FeedEntry
            <$> o .: "url"
            <*> o .: "time"
            <*> o .: "title"
            <*> fmap (markdown def { msXssProtect = False }) (o .: "content")
            <*> pure Nothing
