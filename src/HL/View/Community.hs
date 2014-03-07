{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.View.Community where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Community view.
communityV :: Blaze App
communityV =
  template
    [(CommunityR,"Community")]
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "Community"
                   p []
                     "The Haskell community is spread out online across several mediums \
                     \and around the world!"
                   ul []
                      (do li [] "The Haskell-Cafe mailing list"
                          li [] "StackOverflow"
                          li [] "G+"
                          li [] "Reddit"
                          li [] "The Wiki")))))
