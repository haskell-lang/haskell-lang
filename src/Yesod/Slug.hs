-- | Make a slug.

module Yesod.Slug (Slug(..)) where

import Data.Text (Text)

-- | Make a slug version of the value.
class Slug a where
  toSlug :: a -> Text
