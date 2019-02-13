# Monad transformers

The [transformers](https://www.stackage.org/package/transformers) package provides
useful control flow extensions for monad stacks.

Even without understanding how Monad transformers work, the following should
demonstrate their practicality.

Let's first describe the most simple and useful of the transformers
provided.

## MaybeT

Suppose we have the following functions:

```haskell
getA :: Monad m => m (Maybe a)
getB :: Monad m => Int -> m (Maybe b)
getC :: Monad m => m c
```

And suppose we would like to define a function that tries getting A and B,
and also gets C twice under this monad. If any of the `getA` or `getB`
return `Nothing`, we would like to return `Nothing`.

Working plainly, without using transformers, we would do the following:

```haskell
someCode :: Monad m => m (Maybe (a, b, c, c))
someCode = do
    c1 <- getC
    ma <- getA
    case ma of
        Nothing -> return Nothing
        Just a -> do
            mb <- getB 2
            case mb of
                Nothing -> return Nothing
                Just b -> do
                    c2 <- getC
                    return $ Just (a, b, c1, c2)
```

Using the `MaybeT` transformer we can compact it:


```haskell
import Control.Monad.Trans.Maybe

someCode :: Monad m => m (Maybe (a, b, c, c))
someCode = do
    c1 <- getC
    runMaybeT $ do
        a <- MaybeT getA
        b <- MaybeT $ getB 2
        c2 <- lift getC
        return (a, b, c1, c2)
```

The `runMaybeT` wrapping guarantees that if a monadic function returns
`Nothing`, execution of that monad stops and the whole operation returns
`Nothing`.

Notice that under `MaybeT` every monadic bind needs to be wrapped in a
`MaybeT`, so for functions that return a `Maybe`, simply wrapping them
with `MaybeT` works, and then we don't need to add a `case` for their
returned value later.

However, proxying functions not originally under `MaybeT`, which also
don't return a `Maybe` type requires the use of `lift`. Also, notice
that we did not require a `Just` to wrap the value in the final `return`.

This is useful for instance, when dealing with the many library functions
that return a `Maybe` under `IO`. For example:

```haskell
findExecutable :: String -> IO (Maybe FilePath)

ourFindExes :: MonadIO m => m (Maybe (FilePath, FilePath, FilePath))
ourFindExes = do
    a <- MaybeT $ findExecutable "a-exe"
    b <- MaybeT $ findExecutable "b-exe"
    c <- MaybeT $ findExecutable "c-exe"
    return (a, b, c)
```

### guard

We can use `guard` to check for conditions. If the condition fails,
`Nothing` is returned.

```haskell
ourFindExes :: MonadIO m => m (Maybe (FilePath, FilePath, FilePath))
ourFindExes = do
    a <- MaybeT $ findExecutable "a-exe"
    guard $ a == "/some/path"
    b <- MaybeT $ findExecutable "b-exe"
    c <- MaybeT $ findExecutable "c-exe"
    return (a, b, c)
```

### Pattern matching failures

Pattern matching failures under the `MaybeT` transformers translate
to `Nothing` being returned (rather than exceptions). This is useful
in its own, regardless of whether or not you are using `MaybeT` to
wind down on these calls returning`Maybe`.

For example, observe the extension of the example above, with the
addition of the pattern matching on the return of `getD` and `getE`.

```haskell
getD :: Monad m => m (Maybe (Either Int Char))
getE :: Monad m => m (Either Int Char)

someCode :: Monad m => m (Maybe (a, b, c, c, Int, Char))
someCode = do
    c1 <- getC
    runMaybeT $ do
        a <- MaybeT getA
        b <- MaybeT getB
        Left ld <- MaybeT getD
        Right re <- lift getE
        c2 <- lift $ getC
        return (a, b, c1, c2, ld, re)
```

## ReaderT

The `ReaderT` transformer, available from both the
[transformers](https://www.stackage.org/package/transformers) and
[mtl](https://www.stackage.org/package/mtl) packages, adds a read-only value
which is accessible via `ask` and the helper function `asks`.

In practice, the `ReaderT` transformer is used often for passing down
a static configuration environment, so that such environment need not be
explicitly passed via function parameters.

Similarly to how `runMaybeT` receives a monadic function, `runReaderT`
receives a monadic function, that is also followed by a value, which
can be of any type.

We shall demonstrate an elborate monad stack in the following example:

```haskell
{-# LANGUAGE NoImplicitPrelude        #-}

import ClassyPrelude
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
    let maybeHeadReader = asks headMay
    let runReaderT' = flip runReaderT

    runReaderT' ['2'] $ do
        ask >>= print -- prints: "2"
        asks length >>= print -- prints 1

        runReaderT' [42 :: Int] $ do
            ask >>= print -- prints: [42]

            x <- runMaybeT $ do
                z <- MaybeT maybeHeadReader
                guard $ z == 42
                z2 <- MaybeT $ lift maybeHeadReader
                guard $ z2 == '2'
                return 'x'
            print x -- prints: Just 'x'

            runReaderT' (8.9 :: Double) $ do
                ask >>= print -- prints: 8.9
                (lift . lift) ask >>= print -- prints: "2"
                lift ask >>= print -- prints: [42]

            lift ask >>= print -- prints: "2"

            -- Run with a modified environment
            withReaderT (fmap Just) $ do
                ask >>= print -- prints: [Just 42]
```

Notes about the example above:

* Because the parameters to `runReaderT` are the monadic function
followed by the environment, it helps on convenience to flip the
monadic function to the second arguement, so a `do` can follow, hence
`let runReaderT' = flip runReaderT`.
* Notice the use of
[classy-prelude](https://www.stackage.org/package/classy-prelude) to
provide us with `print`, of the more general type `(Show a, MonadIO m)
=> a -> m ()`, instead of `Show a => a -> IO ()`, so we can freely use
anywhere inside our monad stack.
* We also use `headMay` from `classy-prelude`.
* We allow the nested use of readers, with `lift` used to access the
various levels of the stack.
* We can use `MaybeT` nested inside our stacks of `ReaderT`, and
access the various `ReaderT` levels inside it using `lift`.
* The use of `withReaderT` to run under a modified environment is
demonstrated.

## EitherT

The `EitherT` transformer, available from
[transformers](https://www.stackage.org/package/either) can be
thought of as an extension of `MaybeT`, where instead of `Nothing`
to break out the computation, we can provide a value of a decided
type.

The following provides six examples to demonstrate the use of `EitherT`.

```haskell
{-# LANGUAGE NoImplicitPrelude        #-}

import ClassyPrelude
import Control.Monad.Trans.Either

getE :: Monad m => m (Either a Int)
getE = return $ Right 42

main :: IO ()
main = do
    v0 <- runEitherT $ do
        print "Print this"
        left 2 -- Stopping here!
        print "Will not be printed"
        left 3 -- Type of value passed to all left's must agree,
               -- e.g. left [3] will have failed type checking in
               -- this simple case.
        print "Will also not be printed"
        right 'x' -- Note that: right == return
    print v0 -- prints: Left 2


    v1 <- runEitherT $ do
        -- Incorporate existing monadic functions returning Either,
        -- using EitherT. The 'z' here is the value from the Right,
        -- if Left did not get returned.
        z <- EitherT getE
        return $ z + 1
    print (v1 :: Either () Int) -- prints: Right 43


    v2 <- runEitherT $ do
        print "Print this"
        right [('x', 2)] -- execution does not interrupt, just
                         -- like 'return ...' in IO ().
                         -- the type is ignored too, and need not
                         -- match the type of the actual 'right'.
        print "Will also print this"
        right 'y'
    -- We need a type signature to give a final type for the Left:
    print (v2 :: Either () Char) -- prints: Right 'y'


    -- Use of 'guard' to exit, like in MaybeT
    v3 <- runEitherT $ do
        print "Print this"
        guard False -- Breaks out, returning the mzero of the left type,
                    -- which needs to implement Monoid.
        print "Will not be printed"
        right 'y'
    print (v3 :: Either () Char) -- prints: Left ()


    -- The `eitherT` utility, is like runEitherT, but transforms the
    -- either value to a single result type, using two monadic
    -- functions.
    v4 <-
        let leftSide a = return ("left " ++ show a)
            rightSide a = return ("right " ++ show a)
         in eitherT leftSide rightSide $ do
                x <- lift $ getArgs
                when (x == ["strange command line arg"]) $ do
                    -- If this was true, the print below
                    -- would have printed 'left \"x\"'
                    left 'x'
                right 4


    -- Demonstrate hoistEither:
    v5 <- runEitherT $ do
        print "Print this"
        -- hoistEither receives an Either and is equivalent
        -- to deciding on 'left' or 'right' based on the value:
        hoistEither $ Left ()
        print "Will not be printed"
        hoistEither $ Right ['x']

    print v5 -- prints: Left ()
```

Also provided as utilities to be used in this transformer, are
the functions: `bracketEitherT`, `bimapEitherT`, `mapEitherT`, and
`swapEitherT`.

### Pattern matching failures

Note that unlike `MaybeT`, pattern matching failures in `do` under
this monad, fail the upper monad.

## ContT

Explaining the `ContT` transformer usually takes time, as it tends
to carry you over into the realms of exception handling and coroutines
implementations.

However, instead of undergoing that, we shall demonstrate a case
where `ContT` is immediately being useful as an enhencement over
`EitherT`, as the latter was demonstrated above.

Suppose we have a nested tree of four `runEitherT`s, where we break
from the most inner `EitherT` into the second inner `EitherT`:

```haskell
main :: IO ()
main = do
    v <- runEitherT $ do
        print "Print this"
        r <- runEitherT $ do
            print "Print this"
            runEitherT $ do
                print "Print this"
                runEitherT $ do
                    print "Print this"
                    lift . lift $ left "x"
                    print "Will not be printed"
            print "Will not be printed"
            right 'y'
        print "Print this too"
        hoistEither r
    print v -- prints: Left "x"
```

Note the rather annoying `lift . lift`. It would get much worse,
suppose if we would have nested six `runEitherT` into each other, and
find ourselves quarreling with a long trail of `lift . lift . lift
. lift ...`, just to break out to the upper level.

From this it emerges that it may be nicer to label the scope from
which we would like to break out. The example above can be rewritten
using `runContT`:

```haskell
{-# LANGUAGE NoImplicitPrelude        #-}

module ContT where

import ClassyPrelude
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Either

main :: IO ()
main = do
    let runContM :: Monad m => ContT r m r -> m r
        runContM = (`runContT` return)

    v <- runEitherT $ do
        print "Print this"
        r <- do
            runContM $ callCC $ \breakOut -> do
                print "Print this"
                callCC $ \_ -> do
                    print "Print this"
                    callCC $ \_ -> do
                        print "Print this"
                        breakOut $ Left "x"
                        print "Will not be printed"

                print "Will not be printed"
                return  $ Right 'y'
        print "Print this too"
        hoistEither r
    print v -- prints: Left "x"
```

We replaced the three internal `runEitherT`s with `callCC`s, which
expect a monadic function receiving an argument, and we used a single
transformer - `runContT` instead of the three. The short-hand
`runContM` to `runContT` is used chiefly to remove some clutter, and
to provide `EitherT`-like functionality from `runContT`. Indeed we
could have also used a shorthand such as `runNoCC x = callCC $ \_ -> x`
to clean up further.

The immediate picture is that `breakOut` being the lambda parameter
to the second scope, serves as a "call away" mechanism to break out
of the context, similarly to `left` in `EitherT` and `break;` in
many imperative languages, or `goto` with a label in C, for instance.
