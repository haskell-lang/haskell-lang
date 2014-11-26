-- | Useful utilities for using Lucid with Yesod.

module Yesod.Lucid
  (module Yesod.Lucid
  ,module Yesod)
  where

import Data.Text (Text)
import Lucid
import Yesod hiding (object,Html,toHtml)

-- | A lucid generator.
type FromLucid a =
  Maybe (Route a) ->
  (Route a -> Text) ->
  Html ()

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: MonadHandler m => FromLucid (HandlerSite m) -> m (Html ())
lucid cont =
  do render <- getUrlRender
     route <- getCurrentRoute
     return
       (cont route render)
