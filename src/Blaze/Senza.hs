-- | Blaze without attribute operators

module Blaze.Senza where

import           Blaze (with,Html)
import qualified Blaze.Elements as E
import           Prelude ()

meta :: [E.Attribute] -> Html
meta = with E.meta

headtitle :: Html -> Html
headtitle = E.title

style :: [E.Attribute] -> Html
style = with E.link

script :: [E.Attribute] -> Html -> Html
script = with E.script

div :: [E.Attribute] -> Html -> Html
div = with E.div

span :: [E.Attribute] -> Html -> Html
span = with E.span

link :: [E.Attribute] -> Html
link = with E.link

a :: [E.Attribute] -> Html -> Html
a = with E.a

h1 :: [E.Attribute] -> Html -> Html
h1 = with E.h1

h2 :: [E.Attribute] -> Html -> Html
h2 = with E.h2

h3 :: [E.Attribute] -> Html -> Html
h3 = with E.h3

h4 :: [E.Attribute] -> Html -> Html
h4 = with E.h4

h5 :: [E.Attribute] -> Html -> Html
h5 = with E.h5

head :: [E.Attribute] -> Html -> Html
head = with E.head

body :: [E.Attribute] -> Html -> Html
body = with E.body

nav :: [E.Attribute] -> Html -> Html
nav = with E.nav

ul :: [E.Attribute] -> Html -> Html
ul = with E.ul

ol :: [E.Attribute] -> Html -> Html
ol = with E.ol

li :: [E.Attribute] -> Html -> Html
li = with E.li

p :: [E.Attribute] -> Html -> Html
p = with E.p
