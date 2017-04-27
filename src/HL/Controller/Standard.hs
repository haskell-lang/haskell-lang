{-# LANGUAGE OverloadedStrings #-}
module HL.Controller.Standard
    ( getFaviconR
    , getRobotsR
    , getSitemapR
    ) where

import Data.Conduit (yield)
import Data.Map.Strict (keys)
import HL.Controller
import Yesod.Sitemap

getFaviconR :: C ()
getFaviconR = sendFile "image/x-icon" "static/img/favicon.ico"

getRobotsR :: C Text
getRobotsR = robots SitemapR

getSitemapR :: C TypedContent
getSitemapR = do
    tutKeys <- fmap (keys . appTutorials) getYesod
    sitemap $ do
        yield SitemapUrl
            { sitemapLoc = HomeR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 1
            }
        yield SitemapUrl
            { sitemapLoc = AnnouncementsR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Just Monthly
            , sitemapPriority = Just 0.5
            }
        yield SitemapUrl
            { sitemapLoc = GetStartedR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 1
            }
        yield SitemapUrl
            { sitemapLoc = CommunityR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.7
            }
        yield SitemapUrl
            { sitemapLoc = IrcR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.7
            }
        yield SitemapUrl
            { sitemapLoc = MailingListsR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.7
            }
        yield SitemapUrl
            { sitemapLoc = SuccessStoriesR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.7
            }
        yield SitemapUrl
            { sitemapLoc = DocumentationR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.9
            }
        yield SitemapUrl
            { sitemapLoc = LibrariesR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.9
            }
        yield SitemapUrl
            { sitemapLoc = NewsR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Just Daily
            , sitemapPriority = Just 0.5
            }
        yield SitemapUrl
            { sitemapLoc = InteroR
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.5
            }
        forM_ tutKeys $ \tutKey -> yield SitemapUrl
            { sitemapLoc =
                case tutKey of
                  PackageTutorial pkg -> LibraryR pkg
                  RegularTutorial slug -> TutorialR slug
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.8
            }
