{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import qualified Data.Map as Map
import Data.Monoid ((<>))
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
    let title = tutorialTitle tutorial
        mgithubUrl =
            (\filename ->
                  "https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/" <>
                  filename) <$>
            tutorialLocalFilename tutorial
        !html = do
            forM_ mgithubUrl $
                \githubUrl ->
                     p_
                         (a_
                              [href_ githubUrl]
                              (b_ "View and edit this tutorial on Github"))
            renderMarkdown (tutorialContent tutorial)
    lucid (markdownV title html)
