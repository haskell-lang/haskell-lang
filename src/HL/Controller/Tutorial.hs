{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tutorial page.

module HL.Controller.Tutorial where

import Control.Monad.Catch (catchIOError)
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
import HL.Controller
import HL.View
import HL.View.Markdown
import HL.Model.Markdown

-- | Get mailing lists.
getTutorialR :: Text -> C (Html ())
getTutorialR slug = do
    let fp = "static/tutorial/" ++ T.unpack slug ++ ".md"
    bs <- io (S.readFile fp) `catchIOError` \_ -> notFound
    let text = decodeUtf8With lenientDecode bs
        title = getTitleSimple text
        crumbs = [DocumentationR, TutorialR slug]
        !html = renderMarkdown (Markdown text)

    lucid (markdownV crumbs title html)

-- | A simple approach to getting the title from a Markdown file
getTitleSimple :: Text -> Text
getTitleSimple = T.strip . T.takeWhile (/= '\n') . T.dropWhile (== '#')
