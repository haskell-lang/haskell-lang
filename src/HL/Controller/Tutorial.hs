{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import qualified Data.Map as Map
import Data.Monoid ((<>))
import HL.Controller
import HL.View
import HL.View.Markdown
import HL.View.Template
import HL.Model.Markdown

-- | Get a list of tutorials
getTutorialsR :: C (Html ())
getTutorialsR = do
    tutorials <- fmap appTutorials getYesod
    lucid
        (template
             "Tutorials"
             (container_
                  (row_
                       (span12_
                            [class_ "col-md-12"]
                            (do h1_ "Tutorials"
                                let githubUrl = "https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/"
                                p_ (a_ [href_ githubUrl] (b_ "Edit and create tutorials on Github"))
                                ul_
                                    (mapM_
                                         renderTutorial
                                         (Map.toList tutorials)))))))
  where
    renderTutorial :: (TutorialKey, Tutorial) -> View App ()
    renderTutorial (tutorialKey,Tutorial{tutorialTitle = title}) = do
        let route =
              case tutorialKey of
                PackageTutorial pkg -> LibraryR pkg
                RegularTutorial slug -> TutorialR slug
        url <- lift (asks pageRender)
        li_ (a_ [href_ (url route)] (toHtmlRaw title))

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
