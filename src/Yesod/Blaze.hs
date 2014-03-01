-- | Useful utilities for using blaze-html with Yesod.

module Yesod.Blaze
  (module Yesod.Blaze
  ,module Yesod
  ,module Blaze)
  where

import Yesod hiding (object)
import Blaze

type Blaze a = (Route a -> AttributeValue) -> Html

-- | Output some blaze, passes a URL renderer to the continuation.
blaze :: MonadHandler m => ((Route (HandlerSite m) -> AttributeValue) -> b) -> m b
blaze f =
  do render <- getUrlRender
     return (f (toValue . render))
