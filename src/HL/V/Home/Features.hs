{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Features list.

module HL.V.Home.Features where

import HL.V hiding (list)

-- | Features section explains what's notable about Haskell as a
-- language.
features :: Html
features =
  div ! class_ "features" $
      (container
         (do h1 "Features"
             row (do span6 purefunc
                     span6 statically)
             row (do span6 concurrent
                     span6 inference)
             row (do span6 lazy
                     span6 packages)))

-- TODO: Features: tart up the wording.
--
-- Note: these below are me writing out the facts, for myself, rather
-- than putting in a way that newbies will understand. The intention
-- is to put *something* here and then rewrite the bits below to be
-- more "salesy" aand friendly to people who have no idea what's so
-- special about "IO" from any other form of programming, or what a
-- parallel GC is or unification.

purefunc :: Html
purefunc =
  do h2 "Purely functional"
     p "Every function in Haskell is pure. They are functions in the mathematical sense. \
       \Even side-effecting IO operations are but a description of what to do, produced \
       \by pure code. There are no statements or instructions, only expressions. Which \
       \cannot mutate variables, local or global, or access state like time or random \
       \numbers."
     p (a "View examples")

statically :: Html
statically =
  do h2 "Statically typed"
     p "Every expression in Haskell has a type which is determined at compile time. \
       \All the types composed together by function application have to match up. If \
       \they don't, the program will be rejected by the compiler. Types become not \
       \only a form of guarantee, but a language for expressing the construction \
       \of programs."
     p (a "View examples")

concurrent :: Html
concurrent =
  do h2 "Concurrent"
     p "Haskell lends itself well to concurrent programming due to its explicit \
       \handling of effects. Its flagship compiler, GHC, comes with a high-\
       \performance parallel garbage collector and light-weight concurrency \
       \library containing a number of useful concurrency primitives and \
       \abstractions."
     p (a "View examples")

inference :: Html
inference =
  do h2 "Type inference"
     p "You don't have to explicitly write out every type in a Haskell program. \
       \Types will be inferred by unifying every type bidirectionally. But you \
       \can write out types, or ask the compiler to write them for you, for \
       \handy documentation."
     p (a "View examples")

lazy :: Html
lazy =
  do h2 "Lazy"
     p "Functions don't evaluate their arguments. This means that programs \
       \can compose together very well, with the ability to write control \
       \constructs with normal functions, and, thanks also to the purity \
       \of Haskell, to fuse chains of functions together for high \
       \performance."
     p (a "View examples")

packages :: Html
packages =
  do h2 "Packages"
     p "Open source contribution to Haskell is very active with a wide range \
       \of packages available on the public package servers."
     p (a "View examples")
