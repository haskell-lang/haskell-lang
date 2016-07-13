{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import qualified Data.Map as Map
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
                                ul_
                                    (mapM_
                                         renderTutorial
                                         (Map.toList tutorials)))))))
  where
    renderTutorial :: (Text, Tutorial) -> View App ()
    renderTutorial (slug,Tutorial{tutorialTitle = title}) = do
        url <- lift (asks pageRender)
        li_ (a_ [href_ (url (TutorialR slug))] (toHtmlRaw title))

-- | Get a tutorial
getTutorialR :: Text -> C (Html ())
getTutorialR slug = do
    tutorials <- fmap appTutorials getYesod
    tutorial <- maybe notFound return (Map.lookup slug tutorials)

    let title = tutorialTitle tutorial
        !html = renderMarkdown (tutorialContent tutorial)
    lucid (markdownV title html)
