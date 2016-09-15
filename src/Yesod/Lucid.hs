{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utilities for using Lucid with Yesod.

module Yesod.Lucid
  (module Yesod.Lucid
  ,module Yesod
  ,module Control.Monad.Reader)
  where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Text (Text)
import           Lucid
import           Lucid.Base
import           Yesod (ToTypedContent, MonadHandler, ToContent, Route, HandlerSite,
                        TypedContent, HasContentType(..))
import qualified Yesod as Y

-- | Page information that the view renderer typically needs.
data Page y = Page
    { pageRender :: Route y -> Text
    , pageRoute :: Maybe (Route y)
    , pageCrumbs :: [(Route y, Text)]
    }

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: (Y.YesodBreadcrumbs y) => HtmlT (Reader (Page y)) () -> Y.HandlerT y IO (Html ())
lucid m = do
    render <- Y.getUrlRender
    mroute <- Y.getCurrentRoute
    (title, breadcrumbs) <- Y.breadcrumbs
    return
        (runReader
             (do r <- runHtmlT m
                 return (HtmlT (Identity r)))
             (Page
                  render
                  mroute
                  (breadcrumbs ++
                   [ (route, title)
                   | Just route <- [mroute] ])))

instance ToTypedContent (Html ()) where
  toTypedContent m =
    Y.TypedContent (getContentType (Just m))
    (Y.toContent m)

instance ToContent (Html ()) where
  toContent html =
    Y.ContentBuilder (runIdentity (execHtmlT html))
                     Nothing

instance HasContentType (Html ()) where
  getContentType _ = "text/html"
