{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Home page view.

module HL.View.Home where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Home view.
homeV :: Blaze App
homeV =
  template
    [(HomeR,"Home")]
    "Home"
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "Haskell"
                   p []
                     "The Haskell Platform was a comprehensive, robust development \
                     \environment for programming in Haskell. For new users the \
                     \platform makes it trivial to get up and running with a full \
                     \Haskell development environment. For experienced developers, \
                     \the platform provides a comprehensive, standard base for \
                     \commercial and open source Haskell development that maximises \
                     \interoperability and stability of your code."
                   p []
                     "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
                     \Suspendisse vitae aliquet lorem. Praesent sed egestas risus. \
                     \Cras a neque eget dui pharetra feugiat sed vel erat. Vivamus \
                     \magna sapien, congue quis tellus eu, imperdiet sagittis dolor. \
                     \Praesent dolor magna, suscipit in posuere nec, faucibus eu \
                     \velit."))))
