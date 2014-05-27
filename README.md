haskell-lang
=====

Haskell web site.

### Building

Clone the repo:

    $ git clone git@github.com:chrisdone/hl.git

Create an hsenv:

    $ cd hl
    $ hsenv
    $ source .hsenv/bin/activate

Get the right packakge set:

Add the following to your `.hsenv/cabal/config`, replacing the Hackage reference:

   remote-repo: stackage:http://www.stackage.org/stackage/1ba546f8f281c02d135ec3babd86516f726b4453

Update with Stackage packages and grab submodules:

    $ cabal update
    $ git submodule init
    $ git submodule update

Install project:

    $ cabal install . submodules/senza

Done!

## Running

It runs at: http://localhost:1990/

Manually running the binary:

    $ dist/build/hl/hl

Running from inside GHCi:

    > :l DevelMain
    > DevelMain.update

Run this every time you want to restart.

If you use Emacs, you can just bind it to a key:

``` lisp
(define-key html-mode-map [f12] 'haskell-process-reload-devel-main))
```

Just hit f12 to recompile and restart.
