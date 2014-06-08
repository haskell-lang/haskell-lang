{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Features list.

module HL.V.Home.Features where

import HL.V hiding (list)

-- | Features section explains what's notable about Haskell as a
-- language.
features :: Senza
features =
  div [class_ "features"]
      (container
         (do h1 [] "Features"
             row (do span6 [] nulls
                     span6 [] concurrency)
             row (do span6 [] inference
                     span6 [] ecosystem)))

nulls :: Senza
nulls =
  do h2 [] "No More Null Errors"
     p [] "How often do programs crash because of an unexpected null value? \
          \Haskell programs never do! The compiler has your back; \
          \it tells you anytime you forget to properly handle an optional value."
     p [] (a [] "View examples")

concurrency :: Senza
concurrency =
  do h2 [] "Scaling Just Works"
     p [] "You shouldn't have rewrite half your code base when it's time to scale. \
          \Haskell code is thread-safe by default, and the runtime efficiently handles concurrency for you. \
          \Software transactional memory comes standard."
     p [] (a [] "View examples")

inference :: Senza
inference =
  do h2 [] "Concise and Reliable: Pick Two"
     p [] "What if you didn't have to write out type signatures, \
          \but the compiler still caught all your type errors for you ahead of time? \
          \Haskell's type inference ensures your logs remain completely free \
          \of type errors, while you write signatures—or have the compiler write them for you—\
          \only if you think they'll be helpful."
     p [] (a [] "View examples")

ecosystem :: Senza
ecosystem =
  do h2 [] "Rock-Solid Ecosystem"
     p [] "Haskell code has a reputation: if it compiles, it usually just works. \
          \Bugs are fewer and farther between. \
          \Hackage hosts thousands of such libraries and packages, \
          \any of which you can add to your project with one line of configuration."
     p [] (a [] "View examples")
