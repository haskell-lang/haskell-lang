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
         p [class_ "muted"] "Coming soon."
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
                  " or"
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
             row (do span6 []
                           (do h2 [] "Purely functional"
                               p [] lorem
                               p [] (a [] "View examples"))
                     span6 []
                           (do h2 [] "Statically typed"
                               p [] lorem
                               p [] (a [] "View examples")))
             row (do span6 []
                           (do h2 [] "Concurrent"
                               p [] lorem
                               p [] (a [] "View examples"))
                     span6 []
                           (do h2 [] "Type inference"
                               p [] lorem
                               p [] (a [] "View examples")))
             row (do span6 []
                           (do h2 [] "Lazy"
                               p [] lorem
                               p [] (a [] "View examples"))
                     span6 []
                           (do h2 [] "Packages"
                               p [] lorem
                               p [] (a [] "View examples")))))
  where
    lorem =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean viverra nisl non elit consectetur sodales. Ut condimentum odio in augue scelerisque, eget ultricies arcu placerat. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Mauris a blandit purus, vitae tincidunt leo. "

events :: Senza
events =
  return ()
