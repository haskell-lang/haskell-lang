# Software Transactional Memory

Software Transactional Memory, or STM, is a technique for storing mutable
variables in Haskell. Unlike other mutable variables in Haskell, or mutable
variables in most languages, it provides transactional capabilities. This is
best understood by looking at a typical failure case in other mutable variable
techniques:

```
runServer (|request| => {
  from := accounts.lookup(request.from)
  to := accounts.lookup(request.to)
  accounts.set(request.from, from - request.amt)
  accounts.set(request.to, to + request.amt)
})
```

This kind of pseudo-code is vulnerable to a major race condition, where two
threads both read an account's current value at the same time, and then update
the value, the second thread overwriting changes by the first. This kind of
problem is usually solved by locking, which brings on its own concern about
deadlocks. Instead, let's see how this is accomplished with STM:

```haskell
runServer $ \request -> atomically $ do
  let fromVar = lookup (from request) accounts
      toVar = lookup (to request) accounts
  origFrom <- readTVar fromVar
  writeTVar fromVar (origFrom - amt request)
  origTo <- readTVar toVar
  writeTVar toVar (origTo + amt request)
```

Notice that we're doing the same style of updates as the bad example. However,
with STM, this is safe. Within `atomically`, all reads and writes to a
transactional variable&mdash;or `TVar`&mdash;are tracked. At the end of the
transaction, the runtime checks if any of those variables have been updated
since we looked at them.

If not, it commits the transaction, updating all of the variables at once in a
consistent state. If some variables were changed by another thread in the
interim, our results are thrown away, and the transaction is retried. Thanks to
Haskell's purity, we know that no side effects occur within the `atomically`
call, making this a safe operation.

## Basics

STM comes with a few basic types and operations, and builds a rich
ecosystem from them.

* `STM` is a monad in which all STM actions take place. It allows
  actions which read from and write to `TVar`s, but not other side
  effects (like writing to a file) which cannot be rolled back.
* `atomically :: STM a -> IO a` runs a block of STM actions
  atomically. Either everything succeeds, or nothing does. Since the
  resulting changes to mutable variables are visible to the entire
  program, it must be run from `IO`.
* `TVar` is a mutable variable, which can hold any data type.
* A standard, expected set of `TVar` creation and modification
  functions: `newTVar`, `readTVar`, and `writeTVar`.

These types and functions, along with many more, are exposed from the
`Control.Concurrent.STM` module in the
[stm package](https://www.stackage.org/package/stm).

__EXERCISE__ Fill out the implementation of the following program so
that it gives the output provided below.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
import Control.Concurrent.STM
import Control.Monad (replicateM_)

makeCounter :: IO (IO Int)
makeCounter = do
  var <- newTVarIO 1
  return undefined

main :: IO ()
main = do
  counter <- makeCounter
  replicateM_ 10 $ counter >>= print
```

Should print:

```
1
2
3
4
5
6
7
8
9
10
```

## Failure, retry, and alternative

One of the most powerful concepts in STM is the ability to retry. As a
motivating example: let's say we have two `TVar`s, representing the
bank accounts for Alice and Bob. Alice makes $5 every second (pretty
nice!), and wants to give Bob $20. Our code might initially look like:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Say

main :: IO ()
main = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0

  _ <- forkIO $ payAlice aliceVar

  atomically $ do
    currentAlice <- readTVar aliceVar
    writeTVar aliceVar (currentAlice - 20)
    currentBob <- readTVar bobVar
    writeTVar bobVar (currentBob + 20)

  finalAlice <- atomically $ readTVar aliceVar
  finalBob <- atomically $ readTVar bobVar

  sayString $ "Final Alice: " ++ show finalAlice
  sayString $ "Final Bob: " ++ show finalBob

payAlice :: TVar Int -> IO ()
payAlice aliceVar = forever $ do
  threadDelay 1000000
  atomically $ do
    current <- readTVar aliceVar
    writeTVar aliceVar (current + 5)
  sayString "Paid Alice"
```

There are no race conditions, thanks to STM. But this program is still
buggy, at least at the logic bug level. The issue is that we allow
Alice to give money she doesn't have! Let's look at our output:

```haskell
Final Alice: -20
Final Bob: 20
```

Instead, we want to check that Alice's balance is at least $20 before
we let her transfer the money. In order to do this, we just need to
use one new helper function:

```haskell
check :: Bool -> STM ()
check b = if b then return () else retry
```

As the implementation indicates, this function will do nothing if `b`
is true, but will _retry_ if it's false. This is the second bit of
magic in STM. Since STM needs to track all of the variables it has
looked at in order to handle transactions, it knows exactly what led
to its current state. If you call `retry`, you're saying to STM, "I
don't like this result. Run me again when one of the variables I
looked at changed, and I'll decide if things are OK now."

__EXERCISE__ Use the `check` function to modify the program above so
Alice's bank balance is never negative. The output should look
something like this:

```
Paid Alice
Paid Alice
Paid Alice
Paid Alice
Final Alice: 0
Final Bob: 20
```

On top of this retry behavior, STM also implements an `Alternative`
instance which allows us to try a number of different transactions
until one of them succeeds. For example, let's say both Alice and Bob
are trying to give Charlie $20. We can wait to see who sends the money
first.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Say

main :: IO ()
main = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0
  charlieVar <- newTVarIO 0

  payThread aliceVar 1000000 5
  payThread bobVar   1500000 8

  atomically $ transfer 20 aliceVar charlieVar
           <|> transfer 20 bobVar   charlieVar

  finalAlice <- atomically $ readTVar aliceVar
  finalBob <- atomically $ readTVar bobVar
  finalCharlie <- atomically $ readTVar charlieVar

  sayString $ "Final Alice: " ++ show finalAlice
  sayString $ "Final Bob: " ++ show finalBob
  sayString $ "Final Charlie: " ++ show finalCharlie

payThread :: TVar Int -> Int -> Int -> IO ()
payThread var interval amount = void $ forkIO $ forever $ do
  threadDelay interval
  atomically $ do
    current <- readTVar var
    writeTVar var (current + amount)

transfer :: Int -> TVar Int -> TVar Int -> STM ()
transfer amount fromVar toVar = do
  currentFrom <- readTVar fromVar
  check (currentFrom >= amount)
  writeTVar fromVar (currentFrom - amount)
  currentTo <- readTVar toVar
  writeTVar toVar (currentTo + amount)
```

The runtime system will try to transfer money from Alice. If that
fails, it will try to transfer money from Bob. If that fails, it will
wait until either Alice or Bob's account balance changes, and then try
again.

__QUESTION__ Would it be possible to get the desired behavior if
`transfer` called `atomically` itself and returned `IO ()` instead of
`STM ()`? If so, write the program. If not, why not?

## Other helper functions

You may have already discovered in the previous exercises that there
are a number of other helper functions to work with `TVar`s. One
example is:

```haskell
modifyTVar :: TVar a -> (a -> a) -> STM ()
```

If you're accustomed to dealing with thread-safe code, you may expect
this to use a special implementation internally to perform some
locking. But remember: with STM, locking is unnecessary. Instead, this
is nothing more than a convenience function, with the very simple
implementation:

```haskell
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)
```

__EXERCISE__ This modify function is lazy. Can you change it to be
strict?

### Why newTVarIO?

One of the helper functions available is:

```haskell
newTVarIO :: a -> IO (TVar a)
```

It may seem like this should have the obvious implementation of
`atomically . newTVar`. However, this implementation would be bad for
two reasons. The first is that it's inefficient: it requires all of
the machinery for running a transaction, when by its nature we know
that creating a new `TVar` will never fail.

The second is more subtle. Let's say we're going to follow the common
(albeit arguably evil) practice of creating a global mutable
variable. You'll end up with some code that looks like this:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
import Control.Concurrent.STM
import Control.Monad (replicateM_)
import System.IO.Unsafe (unsafePerformIO)

callCount :: TVar Int
callCount = unsafePerformIO $ atomically $ newTVar 0
{-# NOINLINE callCount #-}

someFunction :: IO ()
someFunction = do
  count <- atomically $ do
    modifyTVar callCount (+ 1)
    readTVar callCount
  putStrLn $ "someFunction call count: " ++ show count

main :: IO ()
main = replicateM_ 10 someFunction
```

Besides the nauseating call to `unsafePerformIO`, everything looks
fine. Unfortunately, running this application fails:

```
Main.hs: Control.Concurrent.STM.atomically was nested
```

The issue is that, in order to properly implement STM, you cannot
embed one call to `atomically` inside another call to
`atomically`. "But wait," you exclaim, "the types prevent that from
ever happening! You can't run an `IO` action inside an `STM` action!"
The problem is that `unsafePerformIO` let us do just this. `callCount`
starts as a thunk which, when evaluated, calls `atomically`. And the
first time it's evaluated is at `modifyTVar callCount (+ 1)`, which is
_also_ inside `atomically`!

For these two reasons, the `newTVarIO` function exists. This isn't a
promotion of global variables, but simply an explanation: if you're
going to use them, do them correctly.

There's also a `readTVarIO` function available. This one is present
purely for performance reasons, as reading a `TVar` is always a
non-failing operation.

__EXERCISE__ Fix up the code above so that it doesn't throw an
exception.

__QUESTION__ Is it possible to use `readTVarIO` in this program? Is it
safe? Would it still be safe if we introduced some concurrency?

## Other variable types

`TVar`s are the core variable type in STM. However, as a convenient,
the `stm` library provides a number of other variable types built on
top of it. We'll demonstrate a few here.

### TMVars

## Channels and queues

There are three related variable types in `stm`:

* `TChan` is an unbounded FIFO channel. You write things to one end,
  and read them at the other end.
* `TQueue` is just like a `TChan`, but it doesn't support a concept
  known as _broadcast_, with multiple readers for a single writer. In
  exchange, `TQueue`s are faster. Takeaway: unless you specific need
  broadcast (which in my experience is a rare occurrence), prefer
  queues.
* `TBQueue` is like a `TQueue`, but is also bounded. If more than the
  given amount of values are present in the queue already, further
  writes will `retry` until the queue has been drained.

In addition to these types, the `stm-chans` library provides a number
of additional channel and queue types, including variants which can be
closed. Let's use one of these to explore the basic API and bit, and
implement a concurrent URL downloader in the process.

__NOTE__ This example takes advantage of the wonderful
[async library](https://haskell-lang.org/library/async), which goes
hand-in-hand with STM. Once you've finished this tutorial, it's
strongly advised to go and read about async to get the rest of the
story with concurrency in Haskell. We're also using the
[http-conduit library](https://haskell-lang.org/library/http-client)
for HTTP requests.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (finally)
import Control.Monad (forever, void)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Network.HTTP.Simple

main :: IO ()
main = do
  queue <- newTBMQueueIO 16
  concurrently_
    (fillQueue queue `finally` atomically (closeTBMQueue queue))
    (replicateConcurrently_ 8 (drainQueue queue))

fillQueue :: TBMQueue (String, String) -> IO ()
fillQueue queue = do
  contents <- getContents
  for_ (lines contents) $ \line ->
    case words line of
      [url, file] -> atomically $ writeTBMQueue queue (url, file)
      _ -> error $ "Invalid line: " ++ show line

drainQueue :: TBMQueue (String, String) -> IO ()
drainQueue queue =
    loop
  where
    loop = do
      mnext <- atomically $ readTBMQueue queue
      case mnext of
        Nothing -> return ()
        Just (url, file) -> do
          req <- parseRequest url
          res <- httpLBS req
          BL.writeFile file $ getResponseBody res
```

## Exceptions

You can throw and catch exceptions inside an STM block if desired. The
semantics are the same as catching and handling exception inside `IO`
itself. Inside of `throwIO` and `catch`, you just use `throwSTM` and
`catchSTM`.

## The join trick

There's a nifty little trick you can use when writing STM code. A
common pattern is to want to perform some `IO` at the end of a
transaction. Since you can't run it inside the transaction itself, you
instead run it right after the transaction. For example:

```haskell
addFunds :: TVar Int -> Int -> IO ()
addFunds var amt = do
  new <- atomically $ do
    orig <- readTVar var
    let new = orig + amt
    writeTVar var new
    return new
  putStrLn $ "New amount: " ++ show new
```

Having to break up your logic like that feels wrong, so instead of
simply returning the `new` value, we can instead return an `IO` action
to be run after the block:

```haskell
addFunds :: TVar Int -> Int -> IO ()
addFunds var amt = do
  action <- atomically $ do
    orig <- readTVar var
    let new = orig + amt
    writeTVar var new
    return $ putStrLn $ "New amount: " ++ show new
  action
```

And then we can use the `join` function to clean things up a bit
further:

```haskell
addFunds :: TVar Int -> Int -> IO ()
addFunds var amt = join $ atomically $ do
  orig <- readTVar var
  let new = orig + amt
  writeTVar var new
  return $ putStrLn $ "New amount: " ++ show new
```

## UnliftIO

There are a number of functions in the `stm` package which are
specialized to `IO`, such as `newTVarIO`. If you work with monad
transformers a lot, and would like something generalized to `MonadIO`,
you can instead import the `UnliftIO.STM` module from the
[unliftio library](https://www.stackage.org/package/unliftio). It's
fully compatible with `stm` itself, just providing lifting (and in
some cases *unlifting*) where needed.

## Concurrency

STM is most useful in concurrent applications. The best next step is
to
[read up on the async library](https://haskell-lang.org/library/async).
