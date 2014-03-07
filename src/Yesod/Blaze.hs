-- | Useful utilities for using blaze-html with Yesod.

module Yesod.Blaze
  (module Yesod.Blaze
  ,module Yesod)
  where

import Yesod hiding (object)
import Blaze

-- | A blaze generator.
type Blaze a =
  Maybe (Route a) ->
  (Route a -> AttributeValue) ->
  Html

-- | Output some blaze, passes a URL renderer to the continuation.
blaze :: MonadHandler m => Blaze (HandlerSite m) -> m Html
blaze cont =
  do render <- getUrlRender
     current <- getCurrentRoute
     return
       (cont current
             (toValue . render))
