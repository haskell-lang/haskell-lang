{-# LANGUAGE OverloadedStrings #-}

-- | Bootstrap layout elements. See
-- <http://getbootstrap.com/2.3.2/scaffolding.html> for more
-- information.

module Text.Blaze.Bootstrap
 (
  -- * Containers
  container
 ,containerFluid
 -- * Rows
 ,row
 ,rowFluid
 -- * Spans
 ,span1
 ,span2
 ,span3
 ,span4
 ,span5
 ,span6
 ,span7
 ,span8
 ,span9
 ,span10
 ,span11
 ,span12
 )
  where

import Prelude hiding (div)
import Text.Blaze.Html5
import Text.Blaze.Extra

-- | A grid container.
container :: Html -> Html
container x = div !. "container" $ x

-- | A fluid grid container.
containerFluid :: Html -> Html
containerFluid x = div !. "container-fluid" $ x

-- | A grid row.
row :: Html -> Html
row x = div !. "row" $ x

-- | A fluid grid row.
rowFluid :: Html -> Html
rowFluid x = div !. "row-fluid" $ x

span1 :: Html -> Html
span1 = div !. "span1 col-md-1"

span2 :: Html -> Html
span2 = div !. "span2 col-md-2"

span3 :: Html -> Html
span3 = div !. "span3 col-md-3"

span4 :: Html -> Html
span4 = div !. "span4 col-md-4"

span5 :: Html -> Html
span5 = div !. "span5 col-md-5"

span6 :: Html -> Html
span6 = div !. "span6 col-md-6"

span7 :: Html -> Html
span7 = div !. "span7 col-md-7"

span8 :: Html -> Html
span8 = div !. "span8 col-md-8"

span9 :: Html -> Html
span9 = div !. "span9 col-md-9"

span10 :: Html -> Html
span10 = div !. "span10 col-md-10"

span11 :: Html -> Html
span11 = div !. "span11 col-md-11"

span12 :: Html -> Html
span12 = div !. "span12 col-md-12"
