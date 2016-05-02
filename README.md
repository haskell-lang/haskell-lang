haskell-lang
============

[![Build Status](https://travis-ci.org/haskell-lang/haskell-lang.svg?branch=master)](https://travis-ci.org/haskell-lang/haskell-lang)

## Purpose

This repository contains the code powering the [haskell-lang.org web
site](http://haskell-lang.org/). This website is a community driven website for
the Haskell programm language. Its primary motivators are:

* A destination that Haskellers can happily send non-Haskellers to in order to
  discover Haskell and get started with modern best practices.
* A source of information for existing Haskellers to discover up-to-date
  information
* A collection of links to various online communities
* A hub for sharing and hosting documentation for all levels of Haskellers

This list is not meant to be exclusive. If there are other ideas people have
for content on this website, feel free to raise it in [the issue
tracker](https://github.com/haskell-lang/haskell-lang/issues/new), [on
Reddit](https://www.reddit.com/r/haskell_lang), or [on
Twitter](https://twitter.com/haskell_lang).

## Contributing content

We're going to make a more straight-forward story for contributing
content changes that should simply involve editing markdown files
under `static/markdown/`.

## Building

Clone the repo:

    $ git clone git@github.com:haskell-lang/haskell-lang.git

Install dependencies and build:

    $ stack build

Done!

### Note on text-icu

If `text-icu` fails to build, you'll want to install the icu dev libraries. The appropriate include/lib directories are configured in the `stack.yaml` already.

* Mac OS X: `brew install icu4c`
* Ubuntu/Debian: `$ sudo apt-get install libicu-dev`

## Running

It runs at: http://localhost:1990/

* Manually running the binary: `$ stack exec haskell-lang`
* Running from inside GHCi: Load `DevelMain` and run
  `DevelMain.update`

## Architecture

It uses Yesod and an MVC organization.

* HL.Model.* -- [models](https://github.com/haskell-lang/haskell-lang/tree/master/src/HL/Model)
* HL.View.* -- [views](https://github.com/haskell-lang/haskell-lang/tree/master/src/HL/View)
* HL.Controller.* -- [controllers](https://github.com/haskell-lang/haskell-lang/tree/master/src/HL/Controller)

Templates are written in
[Lucid](https://github.com/chrisdone/lucid). There is presently no
database.
