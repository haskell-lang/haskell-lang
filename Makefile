build:
	stack build

run: build
	stack exec haskell-lang

ghci:
	stack ghci
