{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utilities for using Lucid with Yesod.

module Yesod.Lucid
  (module Yesod.Lucid
  ,module Yesod)
  where

import           Control.Monad.Identity
import           Data.Text (Text)
import           Lucid
import           Yesod (ToTypedContent, MonadHandler, ToContent, Route, HandlerSite,
                        TypedContent, HasContentType(..))
import qualified Yesod as Y

-- | A lucid generator.
type FromLucid a =
  Maybe (Route a) ->
  (Route a -> Text) ->
  Html ()

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: MonadHandler m => FromLucid (HandlerSite m) -> m (Html ())
lucid cont =
  do render <- Y.getUrlRender
     route <- Y.getCurrentRoute
     return
       (cont route render)

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
