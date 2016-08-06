The [transformers](https://www.stackage.org/package/transformers) package provides 
useful control flow extensions for monad stacks.

Even without understand how Monad transformers work, the following should
demonstrate their practicality.

Let's first describe the most simple and useful of the transforemers
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

Using the MaybeT transformer we can compact it:


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
