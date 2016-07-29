{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Monoid ((<>))
import Network.URI (isUnescapedInURIComponent, escapeURIString)
import HL.Controller
import HL.View
import HL.View.Markdown
import HL.Model.Markdown

-- | Get a tutorial
getTutorialR :: Text -> C (Html ())
getTutorialR = displayTutorial . RegularTutorial

displayTutorial :: TutorialKey -> C (Html ())
displayTutorial tutorialKey = do
    tutorials <- fmap appTutorials getYesod
    let mtutorial = Map.lookup tutorialKey tutorials
    tutorial <-
        case mtutorial of
            Nothing ->
                case tutorialKey of
                    PackageTutorial name -> redirect $ "https://www.stackage.org/package/" <> toPathPiece name
                    RegularTutorial _ -> notFound
            Just tutorial -> return tutorial

    Just route <- getCurrentRoute
    render <- getUrlRender
    let currentUrl = render route
        escapedUrl = T.pack (escapeURIString isUnescapedInURIComponent (T.unpack currentUrl))

    let title = tutorialTitle tutorial
        mgithubUrl =
            (\filename ->
                  "https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/" <>
                  filename) <$>
            tutorialLocalFilename tutorial
        !html = do
            p_ (do forM_ mgithubUrl $ \githubUrl ->
                        (do a_
                                [href_ githubUrl]
                                (b_ "View and edit this tutorial on Github")
                            " | ")

                   -- c/o http://yannesposito.com/Scratch/en/blog/Social-link-the-right-way/
                   a_ [href_ ("https://www.reddit.com/submit?url=" <> escapedUrl)] "Discuss on Reddit"
                   " | "
                   a_ [href_ ("https://twitter.com/home?status=" <> escapedUrl)] "Tweet this")
            renderMarkdown (tutorialContent tutorial)
    lucid (markdownV title html)
