-- | Useful utilities for using Blaze with Yesod.

module Yesod.Blaze
  (module Yesod.Blaze
  ,module Yesod)
  where

import Text.Blaze.Html5
import Yesod hiding (object)

-- | A blaze generator.
type FromBlaze a =
  Maybe (Route a) ->
  (Route a -> AttributeValue) ->
  Html

-- | Output some blaze, passes a URL renderer to the continuation.
blaze :: MonadHandler m => FromBlaze (HandlerSite m) -> m Html
blaze cont =
  do render <- getUrlRender
     route <- getCurrentRoute
     return
       (cont route (toValue . render))
