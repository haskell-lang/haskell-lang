# Asynchronous and Concurrent Programming

The [async package](https://www.stackage.org/package/async) provides
functionality for performing actions asynchronously, across multiple threads.
While it's built on top of the `forkIO` function from base (in
`Control.Concurrent`), the async package improves on this in many ways:

* It has graceful and thorough handling of exceptions
* It builds in a way to get results back from a child thread
* There is an `STM` interface for accessing thread results, providing for a
  convenient way to deal with such things as blocking operations waiting for
  results
* Thread cancelation is made easy and reliable
* For some very common use cases, the `race` and `concurrently` functions, as
  well as the `Concurrently` newtype wrapper, can give you a huge bang for your
  buck.

## Concepts

There is little cognitive overhead to using this package. The primary datatype
it exposes is `Async`. An `Async a` value represents a separate thread which
will ultimately generate a value of type `a`. The package follows some pretty
standard naming conventions:

* `async*` forks a thread and returns an `Async` value
* `withAsync*` forks a thread and provides the `Async` value to the provided
  inner action. The forked thread is killed when the inner action exits.
* You can `wait` for the result of an `Async`, `poll` to check if it's
  complete, or `cancel` it
* By default, `wait`ing will rethrow any exceptions thrown by the forked
  thread. Use the variants that say `Catch` to catch the exceptions.
* Many wait operations have `STM` variants to make them more easily composable

## Concurrently

To warm up, we'll start with examples of using `concurrently`, `race`, and the
`Concurrently` newtype. These are simpler (and more efficient) variants of the
more general `Async`-based interface. The general rule with this library is: if
you can get away with `concurrently`/`race`/`Concurrently`, you should.

### Basics

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Concurrent
import Control.Concurrent.Async

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

main :: IO ()
main = do
    res <- concurrently action1 action2
    print (res :: (Int, String))
```

As you can see, the `concurrently` function waits until both operations
complete, and then returns both results in a tuple. In contrast, the `race`
function returns only the first one to complete:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Concurrent
import Control.Concurrent.Async

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

main :: IO ()
main = do
    res <- race action1 action2
    print (res :: Either Int String)
```

Exercise: what will this program output?

The `Concurrently` newtype wrapper uses the `concurrently` function to
implement its `Applicative` instance, and `race` to implement its `Alternative`
instance. We can demonstrate that, though the code will be quite a bit more
verbose:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

main :: IO ()
main = do
    res1 <- runConcurrently $ (,)
        <$> Concurrently action1
        <*> Concurrently action2
    print (res1 :: (Int, String))

    res2 <- runConcurrently
          $ (Left <$> Concurrently action1)
        <|> (Right <$> Concurrently action2)
    print (res2 :: Either Int String)
```

While this seems tedious for such an example, the `Concurrently` newtype can be
great for larger scale cases, such as when we want to discard some results.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Concurrent.Async
import Data.Foldable (traverse_)

type Score = Int
data Person = Person FilePath Score

people :: [Person]
people =
    [ Person "alice.txt" 50
    , Person "bob.txt" 60
    , Person "charlie.txt" 70
    ]

-- | This function returns a unit value that we don't care about. Using
-- concurrently on two such actions would give us ((), ()).
writePerson :: Person -> IO ()
writePerson (Person fp score) = writeFile fp (show score)

-- | Let's write lots of people to their respective files in parallel, instead
-- of sequentially.
writePeople :: [Person] -> IO ()
writePeople = runConcurrently . traverse_ (Concurrently . writePerson)

-- Note: traverse_ is just mapM_ for Applicative instead instead of Monad.
-- Remember, Concurrently is _not_ a Monad instance.

main :: IO ()
main = writePeople people
```

### Exceptions

When either child thread throws an exception, that exception is thrown to the
other thread:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

action1 :: IO Int
action1 = error "action1 errored"

action2 :: IO String
action2 = handle onErr $ do
    threadDelay 500000
    return "action2 completed"
  where
    onErr e = do
        putStrLn $ "action2 was killed by: " ++ displayException e
        throwIO (e :: SomeException)

main :: IO ()
main = do
    res <- concurrently action1 action2
    print res
```

You'll get some interleaving of output most likely since string-based I/O work
character-by-character, but you should get the idea from running this.

Exercises:

* Use `Data.Text.IO` instead of string-based I/O to avoid the interleaved output
* Replace `concurrently` with `race`. What result do you get?

### Companion infinite threads

There's a neat trick you can accomplish with `race` when you want a companion
thread to continue running as long as the main thread is in operation:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

-- | Print successive numbers to stdout. Notice how it returns @a@ instead of
-- @()@. This lets the type system know that, under normal circumstances, this
-- function will never exit.
counter :: IO a
counter =
    let loop i = do
            putStrLn $ "counter: " ++ show i
            threadDelay 1000000
            loop $! i + 1
     in loop 1

-- | This function will continue to run counter with whatever action you've
-- provided, and stop running counter once that action exits. If by some chance
-- counter throws an exception, it will take down your thread as well.
withCounter :: IO a -> IO a
withCounter inner = do
    res <- race counter inner
    case res of
        Left x -> assert False x
        Right x -> return x

-- More succintly
-- withCounter = fmap (either id id) . race counter

main :: IO ()
main = do
    putStrLn "Before withCounter"
    threadDelay 2000000
    withCounter $ do
        threadDelay 2000000
        putStrLn "Inside withCounter"
        threadDelay 2000000
    threadDelay 2000000
    putStrLn "After withCounter"
    threadDelay 2000000
    putStrLn "Exiting!"
```

Exercises:

* Why does the `assert False` never get triggered (aka, why does `race` never
  return a `Left` value)?
* In the "more succinct" version of the code, how does the `either id id`
  accomplish the same job as the pattern matching?
* Extra credit: could you replace one of the `id`s in `either id id` with
  `Data.Void.absurd`?

__Advanced__ While it's nice to be able to run companion threads, it can be
restricting to require that your main thread live in `IO`. Perhaps you want to
have your main thread live in some monad transformer stack on top of `IO`
instead. Using the powerful (and complex) monad-control package, we can capture
the monadic state to make this work.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package async
{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Control

-- | Print successive numbers to stdout. Notice how it returns @a@ instead of
-- @()@. This lets the type system know that, under normal circumstances, this
-- function will never exit.
counter :: IO a
counter =
    let loop i = do
            putStrLn $ "counter: " ++ show i
            threadDelay 1000000
            loop $! i + 1
     in loop 1

-- | This function will continue to run counter with whatever action you've
-- provided, and stop running counter once that action exits. If by some chance
-- counter throws an exception, it will take down your thread as well.
withCounter :: MonadBaseControl IO m => m a -> m a
withCounter inner = control $ \runInIO -> do
    res <- race counter (runInIO inner)
    case res of
        Left x -> assert False x
        Right x -> return x

-- More succintly
-- withCounter = fmap (either id id) . race counter

main :: IO ()
main = do
    putStrLn "Before withCounter"
    threadDelay 2000000
    flip runReaderT "some string" $ withCounter $ do
        liftIO $ threadDelay 2000000
        str <- ask
        liftIO $ putStrLn $ "Inside withCounter, str == " ++ str
        liftIO $ threadDelay 2000000
    threadDelay 2000000
    putStrLn "After withCounter"
    threadDelay 2000000
    putStrLn "Exiting!"
```

## Async

**FIXME** Needs to be written
