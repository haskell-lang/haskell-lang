haskell-lang
============

[![Build Status](https://travis-ci.org/haskell-lang/haskell-lang.svg?branch=master)](https://travis-ci.org/haskell-lang/haskell-lang)

## Purpose

This repository contains the code powering the [haskell-lang.org web
site](http://haskell-lang.org/). This website is a community driven website for
the Haskell programming language. Its primary motivators are:

* A destination that Haskellers can happily send non-Haskellers to in order to
  discover Haskell and get started with modern best practices
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
under `static/markdown/` or `static/tutorial/`.

### Writing tutorials

This site supports both package-specific tutorials, as well as general
purpose tutorials. The content for both of these lives in
`static/tutorial/`. For package-specific tutorials, the base filename
should be `package-<packagename>`. For example, to write a Markdown
file with content on the vector package, you would edit the file
`static/tutorial/package-vector.md`. The following file extensions are
supported:

* `.md` for local Markdown content. For non-package tutorials, please
  be sure to include a title on the first line, e.g.: `# My Awesome
  Tutorial`
* `.url` for Markdown content hosted elsewhere. The content of this
  file will be a URL where external content can be downloaded.

If this seems overly complex, just check out the
[existing examples in that directory](https://github.com/haskell-lang/haskell-lang/tree/master/static/tutorial).

Please feel free at any time to contribute pull requests to improve
this content.

If you would like to write a new tutorial, please follow this
procedure to "claim" a tutorial and avoid multiple people working on
the same content at once:

* Fork the project
* Create a new branch with a dummy Markdown file for the new content
* Open a pull request titled "Adding tutorial for package `foo`"
* Add your content on your branch
* When you're done writing content, add a comment to the pull request
  indicating that it's ready for review

Before starting on new content, you should make sure that no pull
request already exists for that topic.

## Building

Run these steps:

* Clone the repo: `$ git clone git@github.com:haskell-lang/haskell-lang.git`
* Install dependencies and build: `$ stack build`

If `text-icu` fails to build, you'll want to install the icu dev
libraries (the appropriate include/lib directories are configured in
the `stack.yaml` already):

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

## Continuous Integration

In addition to builds on
[Travis CI](https://travis-ci.org/haskell-lang/haskell-lang), certain branches
are automatically deployed to live websites:

| Branch             | Site                                               |
|--------------------|----------------------------------------------------|
| `master`           | [ci.haskell-lang.org](https://ci.haskell-lang.org) |
| `prod` (protected) | [haskell-lang.org](https://haskell-lang.org)       |
