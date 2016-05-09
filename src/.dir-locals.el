((haskell-mode
  . ((haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-use-ghci . t)
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng" "--no-build" "--no-load"))))
 (haskell-cabal-mode
  . ((haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng")))))
