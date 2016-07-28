{-# LANGUAGE OverloadedStrings #-}
module HL.Controller.Feed
    ( getFeedR
    , FeedEntryWrapper (..)
    , getFeedEntries
    ) where

import Control.Exception.Safe (throwIO)
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import qualified Data.Yaml as Yaml
import HL.Controller
import Text.Markdown (markdown, def, msXssProtect)
import Yesod.Feed


getFeedR :: C TypedContent
getFeedR = do
    now <- liftIO getCurrentTime
    render <- getUrlRender
    entries' <- fmap appFeedEntries getYesod
    let entries = filter ((<= now) . feedEntryUpdated . fst) entries'
        updated =
            case entries of
                (entry, _):_ -> feedEntryUpdated entry
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
        , feedEntries = map (\(e, ident) -> e
            { feedEntryLink =
                case feedEntryLink e of
                    Just url -> url
                    Nothing -> render (AnnouncementR ident)
            , feedEntryEnclosure = Nothing
            }) entries
        }

newtype FeedEntryWrapper = FeedEntryWrapper
    { toFeedEntry :: (FeedEntry (Maybe Text), Text)
    }
instance FromJSON FeedEntryWrapper where
    parseJSON = withObject "FeedEntryWrapper" $ \o -> do
        ident <- o .: "ident"
        entry <- FeedEntry
            <$> o .:? "url"
            <*> o .: "time"
            <*> o .: "title"
            <*> fmap (markdown def { msXssProtect = False }) (o .: "content")
            <*> pure Nothing
        return (FeedEntryWrapper (entry, ident))

getFeedEntries :: IO [FeedEntryWrapper]
getFeedEntries = do
    res <- Yaml.decodeFileEither "config/feed-entries.yaml" >>= either throwIO return
    let getIdent (FeedEntryWrapper (_, ident)) = ident
    let m = Map.unionsWith (+) (fmap (\w -> Map.singleton (getIdent w) 1) res)
    forM_ (Map.toList m) $ \(ident, cnt) ->
        when (cnt /= 1) (error ("Duplicate feed ident: " ++ show ident))
    return res
