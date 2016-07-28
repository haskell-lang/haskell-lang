# Synonyms in base

There are a number of cases in the `base` package where the same
functionality exists under two different names. This can occur for a
number of different reasons, but most commonly it's either:

* One version is more general than the other via some typeclass
* A historical name has lingered on

The presence of identical (or nearly identical) functionality under
different names can lead to confusion when reading code. The purpose
of this page is to point out these occurrences to bypass this
confusion.

## concat, mconcat and fold

```haskell
GHC.OldList.concat :: [[a]] -> [a]
Prelude.concat :: Foldable t => t [a] -> [a]
mconcat :: Monoid a => [a] -> a
fold :: (Foldable t, Monoid a) => t a -> a
```

All four of these functions allow us to collapse down a sequence of
values into a single value. The most specific is `GHC.OldList.concat`:
given a list of lists, it combines all of these lists together into a
single list. The most general is `fold`, which leverages two
typeclasses:

* Instead of requiring that the containers be a list, it leverages the
  `Foldable` typeclass to work with many more data structures
* Instead of requiring that the values being combined be lists, it
  generalizes to any instance of `Monoid`

## concatMap and foldMap

```haskell
GHC.OldList.concatMap :: (a -> [b]) -> [a] -> [b]
Prelude.concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
foldMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
```

This is very similar to the `concat`/`mconcat`/`fold` breakdown
above. We can generalize from lists to instances of `Foldable` and
`Monoid`.

## *> and >>

```haskell
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m => m a -> m b -> m b
```

The only difference between these two is `Applicative` vs
`Monad`. This is a holdover from the days when `Applicative` was not a
superclass of `Monad`.

## pure and return

```haskell
pure :: Applicative f => a -> f a
return :: Monad m => a -> m a
```

`return` is `pure` specialized to `Monad`, relevant for the same
superclass reason above.

## map, fmap and liftM

```haskell
map :: (a -> b) -> [a] -> [b]
fmap :: Functor f => (a -> b) -> f a -> f b
liftM :: (Monad m) => (a -> b) -> m a -> m b
```

`map` is specialized to just lists, while `fmap` is generalized to all
`Functor`s. Like `*>` vs `>>`, the presence of `liftM` is just a
holdover from the days when `Functor` was not a superclass of `Monad`.

## traverse\_ and mapM\_

```haskell
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
```

`mapM_` is `traverse_` specialized to `Monad`, relevant for the same
superclass reason above.

## sequenceA\_ and sequence\_

```haskell
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
```

Same `Monad`/`Applicative` specialization.

## traverse and mapM

```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```

Same `Monad`/`Applicative` specialization.

## sequenceA and sequence

```haskell
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
```

Same `Monad`/`Applicative` specialization.

## for and forM

```haskell
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
```

Same `Monad`/`Applicative` specialization.

## <>, ++ and mappend

```haskell
(++) :: [a] -> [a] -> [a]
Data.Semigroup.(<>) :: Semigroup a => a -> a -> a
Data.Monoid.(<>) :: Monoid m => m -> m -> m
mappend :: Monoid m => m -> m -> m
```

The presence of two different `<>` operators is purely a historical
accident, caused by the fact that `Semigroup` is not (yet) a
superclass of `Monoid`. The functionality for each version of the
operator should be identical. In the case of `Monoid`, the `<>`
operator is identical to the `mappend` function.

`++` is simply `<>` and `mappend` specialized to lists.
