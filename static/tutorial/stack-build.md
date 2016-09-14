# How to Build with Stack

This tutorial assumes you've already installed the Stack build
tool. If you haven't yet, please start with the
[get started page](/get-started) and then come back here.

You may also be interested in learning:

* [How to Play with Stack](stack-play)
* [How to Script with Stack](stack-script)

In this article we'll be talking about how to build full fledged
projects consisting of libraries, executables, test suites, and
benchmarks in Haskell with Stack.

## Start a new project

You can start a new project with the command:

    stack new my-project-name

This will use the default project template. You can also choose from
many available templates which you can list with

    stack templates

For example, if you wanted to the `simple` template instead, you could
run:

    stack new my-project-name simple

You should now have a directory named `my-project-name` (or whatever
string you used), with a `stack.yaml` file inside of it. A
`stack.yaml` file must be present in the root directory of each
project, and provides a number of settings. You can edit that file and
view the comments, or
[view the configuration file documentation](https://docs.haskellstack.org/en/stable/yaml_configuration/).

## Convert an existing package

If you have an existing Cabal package, you can use the `stack init`
command inside the package directory to initialize a `stack.yaml`
file. Stack will attempt to determine a package set compatible with
the packages requested in your `.cabal` file.

## Project versus package

Note the difference in terminology above. This is important: a cabal
package is identified by a single `.cabal` file, and has 0 or 1
libraries, and 0 or more executables, test suites, and benchmarks.

A Stack project has 1 or more cabal packages, and can build them all
at the same time. In likely the majority of cases, your Stack project
will have just one cabal package in it. However, multi-package
projects can be very common for both open source library collections
and for structuring commercial applications. Some open source examples
of multi-package projects include
[Yesod](http://github.com/yesodweb/yesod),
[WAI](https://github.com/yesodweb/wai), and
[Servant](https://github.com/haskell-servant/servant).

## Building

The basic command for building your project is

    stack build

Very likely, you'll need to first tell Stack to install the
appropriate GHC version for your project. You can do this with:

    stack setup

or by using the `--install-ghc` option to `stack build`:

    stack --install-ghc build

## Running executables

Let's suppose your project defines an executable called
`my-executable`. How do you run it? There are three common ways:

1.  `stack exec my-executable` will modify your `PATH` variable to
    include a number of additional directories, including the internal
    executable destination, and your build tools (like `ghc`).

2.  `stack exec which my-executable` will use the `which` command to
    find the full path to your executable, which you could then run,
    without the additional modifications that `stack exec` implies. If
    you want to be clever, you could do something like this from your
    shell:

    ```shell
    $ $(stack exec which my-executable)

3.  The `stack install` command will copy your executables into a
    user-specific directory, such as `$HOME/.local/bin` on POSIX
    systems. The directory will be printed to your console.

## Testing

Testing is also straightforward:

    stack test

As it happens, this is just a convenience shortcut for:

    stack build --test

The same applies to `stack bench` (for benchmarking) and `stack
haddock` (for building Haddock documentation). What this means is that
you can compose these flags to build the code, build the docs, run
tests, and run benchmarks:

    stack build --test --bench --haddock

## Common flags

* `--file-watch` will run build in file-watch mode, where it will wait
  for changes to files and then automatically rebuild. This can be
  very convenient to run in a terminal while simulatenously editing in
  another window.
* `--fast` will disable optimizations
* `--pedantic` turns on `-Wall -Werror` for GHC (all warnings on, and
  warnings treated as errors)

So throwing a few of these together:

    stack build --test --file-watch --fast --pedantic

## What's next

This is a small taste of the capabilities of Stack for building
projects. You can find much more information in the
[Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/).
