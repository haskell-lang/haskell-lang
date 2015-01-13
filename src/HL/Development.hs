{-# LANGUAGE CPP #-}

-- | Development?

module HL.Development where

development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
