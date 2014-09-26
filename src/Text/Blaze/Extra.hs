-- | Extra combinators for blaze.

module Text.Blaze.Extra where


import           Text.Blaze

import qualified Text.Blaze.Html5.Attributes as A

import           Text.Blaze.Internal (Attributable)

-- | Class attribute.
(!.) :: (Attributable h) => h -> AttributeValue -> h
e !. className = e ! A.class_ className

-- | Id attribute.
(!#) :: (Attributable h) => h -> AttributeValue -> h
e !# idName = e ! A.id idName
