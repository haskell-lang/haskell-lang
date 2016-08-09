With optparse-applicative, you declare your program's options in one place, and
get nicely formatted `--help` text and bash auto-completion for free.

API docs
--------

A "getting started" guide and API documentation can be found
[here](https://www.stackage.org/package/optparse-applicative). There's plenty
of good information in the API docs.

Basic example
-------------

Here's an example:

```haskell
import Options.Applicative

data Opts = Opts
    { optFlag :: !Bool
    , optVal :: !String
    }

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn
        (concat ["Hello, ", optVal opts, ", the flag is ", show (optFlag opts)])
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "optparse example" <>
             header
                 "optparse-example - a small example program for optparse-applicative")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions =
        Opts <$> switch (long "some-flag" <> help "Set the flag") <*>
        strOption
            (long "some-value" <> metavar "VALUE" <> value "default" <>
             help "Override default name")
```

Without arguments, this program outputs `Hello, default! The flag is False`. If
you run this program with the `--help` argument, you get this output:

```
optparse example

Usage: optparse-example.hs [--version] [--help] [--some-flag]
                           [--some-value VALUE]
  A small example program for optparse-applicative

Available options:
  --version                Show version
  --help                   Show this help text
  --some-flag              Set the flag
  --some-value VALUE       Override default name
```

Now use the arguments `--some-flag --some-value world`, and the output changes
to: `Hello, world! The flag is True`.

In addition, `--version` will display the version and exit immediately.

Sub-commands example
--------------------

It's also easy to create a program with multiple sub-commands:

```haskell
import Options.Applicative

data Opts = Opts
    { optGlobalFlag :: !Bool
    , optCommand :: !Command
    }

data Command
    = Create String
    | Delete

main :: IO ()
main = do
    opts <- execParser optsParser
    case optCommand opts of
        Create name -> putStrLn ("Created the thing named " ++ name)
        Delete -> putStrLn "Deleted the thing!"
    putStrLn ("global flag: " ++ show (optGlobalFlag opts))
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "optparse subcommands example" <>
             header
                 "optparse-sub-example - a small example program for optparse-applicative with subcommands")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions =
        Opts <$> switch (long "global-flag" <> help "Set a global flag") <*>
        hsubparser (createCommand <> deleteCommand)
    createCommand =
        command
            "create"
            (info createOptions (progDesc "Create a thing"))
    createOptions =
        Create <$>
        strArgument (metavar "NAME" <> help "Name of the thing to create")
    deleteCommand =
        command
            "delete"
            (info (pure Delete) (progDesc "Delete the thing"))
```

Now you can pass the arguments `create foo`, and the output will be:

```
Created the thing named foo
global flag: False
```

In this case the `create` subcommand only has a
single option, but as with global options you can put together multiple option
parsers using the Applicative operators (`Constructor <$> arg1 <*> arg2 <*>
...`).

Require one option out of a group of possibilities
---------------------------------------------------

The parser's Alternative instance lets you require one argument out of a group
of possibilities by separating them with `<|>`. For example, `(strOption (long
"foo") <|> strOption (long "bar"))` requires either the `--foo` _or_ `--bar`
option.

Parsing other option types
--------------------------

So far, we have used `strOption` to parse a simple String option, but this will
not work if your option has a different datatype such as `Int`. For this case,
you can usually use `option auto` instead, which will use the Read instance to
parse the argument, and fail gracefully if it cannot be parsed.

It's also possible to define your own reader and pass it to `option` for custom
parsing. For example, this will parse a date in the "Jun 12, 1977" format:

```haskell
import Data.Time

dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%b %d, %Y" arg of
        Nothing -> Left ("Cannot parse date: " ++ arg)
        Just day -> Right day
```

Then use `option dayReader (long "some-date")` for the option parser.

For options of type `Text`, you have to do things a bit differently. Using
`option auto` would require quotes around the argument value (which, in a shell,
means escaping the quotes like `\"text\"`) since that's what the Read
instance expects. Instead, use the parser's Functor instance to pack a regular
String to Text. For example, `fmap T.pack (strOption (long "some-text"))`.
Better yet, define `textOption`:

```haskell
import qualified Data.Text as T

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption
```

Other parsers and options
-------------------------

These examples have only shown a few of the types of option parsers (`switch`
`strOption`, etc.) and their options (`long`, `help`, etc.), but you can read
the
[haddocks for Options.Applicative.Builder](http://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Builder.html)
for the full list. It's worth browsing through the whole thing to get a sense of
all the possibilities.

Package version number and Git commit ID
----------------------------------------

It's often nice take the version number from your Cabal package rather than have
to repeat it in the source code. To do that, `import Paths_your_package
(version)` (adjust "your_package" to match your package's name). It's also nice
to include the Git commit ID of your project in the `--version` output
for traceability, which the [gitrev](https://www.stackage.org/package/gitrev)
library makes easy.

Here's an example that puts them together. Notice that you must enable the
TemplateHaskell extension for `$(gitHash)`.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_optparse_example (version)

main :: IO ()
main = do
    s <- execParser optsParser
    print s
  where
    optsParser = info (helper <*> versionOption <*> strArgument mempty) mempty
    versionOption =
        infoOption
            (concat [showVersion version, " ", $(gitHash)])
            (long "version" <> help "Show version")
```

Now the `--version` output looks like this:
`0.1.0.0 9cc1dbd020c2a42e2bd93204d517470c5781bbf2`.

Using bash auto-completion
--------------------------

Every program using optparse-applicative gets hidden arguments to support bash
auto-completion. For your own personal use, you run this or add it this to your
`.bashrc`:

```sh
eval "$(myprog --bash-completion-script myprog)"
```

To install the bash completion system-wide, put the output of
--bash-completion-script where your system looks for it. For example, on Ubuntu:

```sh
myprog --bash-completion-script myprog >/etc/bash_completion.d/myprog
```
