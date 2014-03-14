-- | A prelude for when using blaze-html.

module Blaze.Prelude
  (module Blaze.Attributes
  ,module Blaze.Senza
  ,module Prelude
  ,AttributeValue
  ,docTypeHtml)
  where

import Blaze.Attributes hiding (style,span)
import Blaze.Senza
import Blaze
import Prelude hiding (head,div,max,span,id,min)
