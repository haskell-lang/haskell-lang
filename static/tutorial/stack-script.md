# How to Script with Stack

This tutorial assumes you've already installed the Stack build
tool. If you haven't yet, please start with the
[get started page](/get-started) and then come back here.

You may also be interested in learning:

* [How to Play with Stack](stack-play)
* [How to Build with Stack](stack-build)

In this article we'll be talking about how to write short scripts and
self-contained programs in Haskell with Stack.

## Accessing the Haskell tools

When you install Stack, the common Haskell tools like `ghc` and `ghci`
do not end up on your PATH. This is to allow you to easily switch
between different GHC versions on projects. The easiest way to access
these tools is to prefix any command with `stack exec --`, e.g.:

    stack exec -- ghc --version

If you haven't installed GHC yet, you'll get an error message about
`ghc` not being found. You can resolve that by running `stack setup`
first, or by adding the `--install-ghc` option to enable automatic GHC
installation:

    stack setup
    # or
    stack --install-ghc exec -- ghc --version

__NOTE__ The `--` after `exec` tells Stack to pass the rest of the
command line arguments to the specified program instead of parsing
them itself.

## Compiling an executable

Let's write a simple program, save it in `HelloWorld.hs`:

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

To compile it, you can run

    $ stack exec -- ghc HelloWorld.hs

This uses the `ghc` executable directly to perform the
compilation. But actually, since `exec ghc` is such a common
combination, we have a convenience shortcut available:

    $ stack ghc -- HelloWorld.hs

You can now run your program with `./HelloWorld`, or on Windows `HelloWorld.exe`.

## Running a script

We don't always want to compile an executable. Sometimes it's
convenient to just use GHC's scripting capabilities. To do that, you
can run:

    $ stack exec -- runghc HelloWorld.hs

Or, as you may have guessed:

    $ stack runghc -- HelloWorld.hs

## Adding packages

Let's say we want to use a package that doesn't ship with GHC itself,
like [http-conduit](https://haskell-lang.org/library/http-client). The
`stack exec` command - and its shortcuts like `stack ghc` and `stack
runghc` - all take a `--package` argument indicating that an
additional package needs to be present. So consider this `http.hs`
file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

You can ensure that it runs with the `http-conduit` package available
by using:

    $ stack runghc --package http-conduit -- http.hs

## Specify a resolver

There's unfortunately a major downside to everything we've seen so
far: there are no guarantees given about which version of GHC or
libraries will be used. The selection will depend entirely on what
Stack deems the most appropriate *resolver* - or collection of GHC and
libraries - when you start using it. If you want to be guaranteed that
a specific set of packages will be used, you can set the `--resolver`
on the command line.

    $ stack --resolver lts-6.15 --install-ghc runghc --package http-conduit http.hs

Note that we also included `--install-ghc` to make sure that the
correct GHC is downloaded and installed if necessary.

## Script interpreter

Remembering to pass all of these flags on the command line is very
tedious, error prone, and makes it difficult to share scripts with
others. To address this, Stack has a
[*script interpreter* feature](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)
which allows these flags to be placed at the top of your script. Stack
also has a dedicated `script` command which has some nice features
like auto-detection of packages you need based on import statements.

If we modify our `http.hs` to say:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.5 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

We can now run it by simply typing:

    $ stack http.hs

Furthermore, on POSIX systems, that line at the top is known as the
[shebang](https://en.wikipedia.org/wiki/Shebang_%28Unix%29). This
means that, if you make your script executable, you can just run it
like a normal program:

    $ chmod +x http.hs
    $ ./http.hs

If you want to create self contained scripts, a script interpreter
line at the top of your file is a highly recommended practice.

## What's next

The intuitions we've built for the `ghc` and `runghc` commands applies
equally well to the `ghci` REPL. This is discussed at more length in
the [How to Play](stack-play) tutorial. The quick summary is: `stack
exec --package http-conduit -- ghci` to get a powerful REPL ready to
make HTTP requests.

While the script interpreter is great for small programs, it doesn't
scale nicely to very large projects. At that point, we recommend you
bump up to a full-blown Stack project, which allows for more complex
configuration via a `stack.yaml` file. You can find more details on
this in [How to Build](stack-build).

There's also a blog post available on writing
[bitrot-free scripts with Haskell](https://www.fpcomplete.com/blog/2016/08/bitrot-free-scripts),
which heavily uses the techniques mentioned here.
