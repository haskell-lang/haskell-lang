haskell-lang
=====

Haskell web site.

## Editing pages

If you're just here to contribute a content change, read this section.

All pages that are produced by markdown
[are here](https://github.com/chrisdone/hl/tree/master/static/markdown). To
contribute changes simply fork this repo and open a pull request. It
will be merged and redeployed in short order.

If you want to edit a page which has some custom code in it, you'll
want to see the next sections for building and running
instructions. Maybe also take a look at [architecture](#architecture).

If you want to include Haskell code samples in markdown, use:

    ``` haskell
    main = putStrLn "Hello, World!"
    ```

If you want to include Haskell code samples in Haskell code pages,
use:

``` haskell
haskellPre "main = print 123"
haskellCode "peyton `simon` jones"
```

Pre for `<pre>` block, code for `<code>` span snippet.

## Building

Clone the repo:

    $ git clone git@github.com:chrisdone/hl.git

You need one of these GHC versions:

* GHC 7.6
* GHC 7.8

Create an [hsenv](http://hackage.haskell.org/package/hsenv):

    $ cd hl
    $ hsenv
    $ source .hsenv/bin/activate

You need the right package set. Add the following to your
`.hsenv/cabal/config`, replacing the Hackage reference:

* If you have GHC 7.6:

        remote-repo: stackage:http://www.stackage.org/stackage/1ba546f8f281c02d135ec3babd86516f726b4453
* If you have GHC 7.8:

        remote-repo: stackage:http://www.stackage.org/stackage/160b97ce7459820d4de720d6a867b85297ab4351

Now just run:

    $ sh scripts/pull-build

To do the above cabal update, submodule, install, etc.

Done!

## Running

It runs at: http://localhost:1990/

Manually running the binary:

    $ dist/build/hl/hl

Running from inside GHCi:

    > :l DevelMain
    > DevelMain.update

Run this every time you want to update the web handler in-place, as in
[this demo](https://github.com/chrisdone/ghci-reload-demo).

If you use Emacs, you can just bind it to a key:

``` lisp
(define-key html-mode-map [f12] 'haskell-process-reload-devel-main)
```

Just hit f12 to recompile and restart.

## Architecture

It uses Yesod and an MVC organization.

* HL.M.* -- [models](https://github.com/chrisdone/hl/tree/master/src/HL/M)
* HL.V.* -- [views](https://github.com/chrisdone/hl/tree/master/src/HL/V)
* HL.C.* -- [controllers](https://github.com/chrisdone/hl/tree/master/src/HL/C)

Templates are written in [senza](https://github.com/chrisdone/senza),
a convenience wrapper to blaze-html. There is presently no database.

## Style

I use [this style](https://github.com/chrisdone/haskell-style-guide)
for code. You don't have to use this style, I can reformat patches as
they come in.
