{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Home/landing page.

module HL.V.Home where

import HL.V hiding (list)
import HL.V.Code
import HL.V.Template

-- | Home view.
homeV :: FromSenza App
homeV =
  skeleton
    "Haskell Programming Language"
    (\_ _ ->
       linkcss "http://fonts.googleapis.com/css?family=Ubuntu:700")
    (\cur url ->
       do navigation False Nothing url
          header url
          try
          community url
          features
          events
          div [class_ "mobile"]
              (navigation False cur url))

header :: (Route App -> AttributeValue) -> Senza
header url =
  div [class_ "header"]
      (container
         (row
            (do span6 []
                      (div [class_ "branding"]
                           (do branding
                               summation))
                span6 []
                      (div [class_ "branding"]
                           (do tag
                               sample)))))
  where branding =
          span [class_ "name"
               ,background url img_logo_png]
               "Haskell"
        summation =
          span [class_ "summary"]
               "An advanced purely-functional programming language"
        tag =
          span [class_ "tag"]
               "Natural, declarative, statically typed code."
        sample = div [class_ "code-sample"]
                     (haskellPre codeSample)

codeSample :: Text
codeSample =
  "primes = sieve [2..]\n\
  \    where sieve (p:xs) = \n\
  \      p : sieve [x | x <- xs, x `mod` p /= 0]"

try :: Senza
try =
  div [class_ "try"]
      (container
         (row
            (do span6 [] repl
                span6 [] rhs)))
  where
    repl =
      do h2 [] "Try it"
         todo "Coming soon."
    rhs =
      do h2 [] "Got 5 minutes?"
         p [] (do "Type "
                  span [class_ "highlight"]
                       "help"
                  " to start an interactive tutorial.")
         p [] "Or try typing these out and see what happens (click to insert):"
         p [] (do haskellCode "23 * 36"
                  " or "
                  haskellCode "reverse \"hello\""
                  " or "
                  haskellCode "foldr (:) [] [1,2,3]"
                  " or "
                  haskellCode "do line <- getLine; putStrLn line"
                  " or "
                  haskellCode "readFile \"/welcome\"")
         p [] (do a [href "https://hackage.haskell.org/package/pure-io-0.2.0/docs/PureIO.html#g:2"]
                    "These"
                  " IO actions are supported in this app.")

community :: (Route App -> AttributeValue) -> Senza
community url =
  div [class_ "community"
      ,background url img_community_jpg]
      (do container
            (do row
                  (span8 []
                         (do h1 []
                                "An open source community effort for over 20 years"
                             p [class_ "learn-more"]
                               (a [href (url CommunityR)]
                                  "Learn more")))))

features :: Senza
features =
  div [class_ "features"]
      (container
         (do h1 [] "Features"
             row (do span6 [] purefunc
                     span6 [] statically)
             row (do span6 [] concurrent
                     span6 [] inference)
             row (do span6 [] lazy
                     span6 [] packages)))

-- Note: these below are me writing out the facts, for myself, rather
-- than putting in a way that newbies will understand. The intention
-- is to put *something* here and then rewrite the bits below to be
-- more "salesy" aand friendly to people who have no idea what's so
-- special about "IO" from any other form of programming, or what a
-- parallel GC is or unification.

purefunc :: Senza
purefunc =
  do h2 [] "Purely functional"
     p [] "Every function in Haskell is pure. They are functions in the mathematical sense. \
          \Even side-effecting IO operations are but a description of what to do, produced \
          \by pure code. There are no statements or instructions, only expressions. Which \
          \cannot mutate variables, local or global, or access state like time or random \
          \numbers."
     p [] (a [] "View examples")

statically :: Senza
statically =
  do h2 [] "Statically typed"
     p [] "Every expression in Haskell has a type which is determined at compile time. \
          \All the types composed together by function application have to match up. If \
          \they don't, the program will be rejected by the compiler. Types become not \
          \only a form of guarantee, but a language for expressing the construction \
          \of programs."
     p [] (a [] "View examples")

concurrent :: Senza
concurrent =
  do h2 [] "Concurrent"
     p [] "Haskell lends itself well to concurrent programming due to its explicit \
          \handling of effects. Its flagship compiler, GHC, comes with a high-\
          \performance parallel garbage collector and light-weight concurrency \
          \library containing a number of useful concurrency primitives and \
          \abstractions."
     p [] (a [] "View examples")

inference :: Senza
inference =
  do h2 [] "Type inference"
     p [] "You don't have to explicitly write out every type in a Haskell program. \
          \Types will be inferred by unifying every type bidirectionally. But you \
          \can write out types, or ask the compiler to write them for you, for \
          \handy documentation."
     p [] (a [] "View examples")

lazy :: Senza
lazy =
  do h2 [] "Lazy"
     p [] "Functions don't evaluate their arguments. This means that programs \
          \can compose together very well, with the ability to write control \
          \constructs with normal functions, and, thanks also to the purity \
          \of Haskell, to fuse chains of functions together for high \
          \performance."
     p [] (a [] "View examples")

packages :: Senza
packages =
  do h2 [] "Packages"
     p [] "Open source contribution to Haskell is very active with a wide range \
          \of packages available on the public package servers."
     p [] (a [] "View examples")

events :: Senza
events =
  return ()
