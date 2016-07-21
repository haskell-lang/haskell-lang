# Operator Glossary

One aspect of Haskell that many new users find difficult to get a
handle on is _operators_. Unlike many other languages, Haskell gives a
lot of flexibility to developers to define custom operators. This can
lead to shorter, more elegant code in many cases. For example, compare
these three equivalent pieces of code:

```haskell
v1 = mappend (mappend ("hello " "there ")) "world"
v2 = "hello " `mappend` "there " `mappend ` "world"
v3 = "hello " ++ "there " ++ "world"
```

Unfortunately, not all operators are as self-explanatory as the `++`
operator (which, in case you're wondering, is "list append"). This
page will attempt to cover the most common "surprising" operators. In
other words: we won't bother covering common mathematical operators
like `+` or `*`, nor will we cover operators defined in less common
libraries.

## Hoogle is your friend

It's worth pointing out as well that for many operators, using the
[Hoogle search engine](https://www.stackage.org/lts/hoogle) can be a
great way to find out about an operator, or for that matter _any_
function. It's
[pretty easy to find `++`](https://www.stackage.org/lts-6.7/hoogle?q=%2B%2B)
that way. Go ahead, try it out now!

<form action="https://www.stackage.org/lts/hoogle" target="_blank"><input type="search" name="q"> <input type="submit" value="Search Hoogle"></form>

## Function application `$`

```haskell
($) :: (a -> b) -> a -> b
```

One of the most common operators, and source of initial confusion, is
the `$` operator. All this does is _apply a function_. So, `f $ x` is
exactly equivalent to `f x`. If so, why would you ever use `$`? The
primary reason is - for those who prefer the style - to avoid
paratheses. For example, you can replace:

```haskell
foo (bar (baz bin))
```

with

```haskell
foo $ bar $ baz bin
```

A less common but arguable more compelling use case is to capture the
act of applying a function to an argument. To clarify that rather
vague statement with an example:

```haskell
#!/usr/bin/env stack
-- stack --resolver ghc-7.10.3 runghc

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()
main = print (map ($ 5) [double, square])
```

The `($ 5)` bit means "apply the function to 5", and then we can use
`map` to use it with both the `double` and `square` functions.

## Function composition `.`

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Not much more to it than that: take two functions and compose them together.

```haskell
#!/usr/bin/env stack
-- stack --resolver ghc-7.10.3 runghc

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()
main = (print . double . square) 5
```

Or you can combine this together with the `$` operator to avoid those
parantheses if you're so inclined:

```haskell
main = print . double . square $ 5
```

In addition to its usage for function composition, the period is also used for hierarchical modules, e.g.:

```haskell
#!/usr/bin/env stack
-- stack --resolver ghc-7.10.3 runghc
import qualified Data.Monoid

main :: IO ()
main = putStrLn $ Data.Monoid.mappend "hello " "world"
```

Finally, in the `Control.Category` module, the `Category` typeclass
_also_ uses the `.` operator to define categorical composition. This
generalizes standard function composition, but is not as commonly
used.

## Monoidal append `<>`

```haskell
(<>) :: Monoid m => m -> m -> m
```

The `<>` operator is just a synonym for the
[`mappend` function](https://www.stackage.org/haddock/lts-6.7/base-4.8.2.0/Prelude.html#v:mappend). This
comes from the `Monoid` typeclass, which represents types which have
an identity and an associative binary operation. Some examples:

* For lists, `<>` is the same as `++` (append two lists)
* For [vectors](https://haskell-lang.org/package/vector), this logic
  holds as well
* For
  [`Set`](https://www.stackage.org/haddock/lts-6.7/containers-0.5.6.2/Data-Set.html)s,
  this is a union operation (all values present in either `Set`)
* For
  [`Map`](https://www.stackage.org/haddock/lts-6.7/containers-0.5.6.2/Data-Map-Strict.html)s,
  we have a "left biased union", meaning we combine the key/value
  pairs from both inputs, and if both inputs share a key, the value in
  the left input is selected
* For numbers, both addition and multiplication form a `Monoid`, where
  0 is the additive identity (since `0 + x = x`) and 1 is the
  multiplicative identity (since `1 * x = x`). Therefore, to avoid
  confusion, `Data.Monoid` defines helper newtype wrappers
  [`Sum`](https://www.stackage.org/haddock/lts-6.7/base/Data-Monoid.html#t:Sum)
  and
  [`Product`](https://www.stackage.org/haddock/lts-6.7/base/Data-Monoid.html#t:Product)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.4 runghc
import Data.Monoid ((<>))

main :: IO ()
main = putStrLn $ "hello " <> "there " <> "world!"
```

## Functor map `<$>`

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$) :: Functor f => a -> f b -> f a
($>) :: Functor f => f a -> b -> f b
```

The `<$>` operator is just a synonym for the
[`fmap` function](https://www.stackage.org/haddock/lts-6.7/base-4.8.2.0/Prelude.html#v:fmap)
from the `Functor` typeclass. This function generalizes the `map`
function for lists to many other data types, such as `Maybe`, `IO`,
and `Map`.

```haskell
#!/usr/bin/env stack
-- stack --resolver ghc-7.10.3 runghc
import Data.Monoid ((<>))

main :: IO ()
main = do
    putStrLn "Enter your year of birth"
    year <- read <$> getLine
    let age :: Int
        age = 2020 - year
    putStrLn $ "Age in 2020: " <> show age
```

In addition, there are two additional operators provided which replace
a value inside a `Functor` instead of applying a function. This can be
both more convenient in some cases, as well as for some `Functor`s be
more efficient. In terms of definition:

```haskell
value <$ functor = const value <$> functor
functor $> value = const value <$> functor

x <$ y = y $> x
x $> y = y <$ x
```

## Applicative function application `<*>`

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a
```

Commonly seen with `<$>`, `<*>` is an operator that applies a wrapped
function to a wrapped value. It is part of the `Applicative`
typeclass, and is very often seen in code like the following:

```haskell
foo <$> bar <*> baz
```

For cases when you're dealing with a `Monad`, this is equivalent to:

```haskell
do x <- bar
   y <- baz
   return (foo x y)
```

Other common examples including parsers and serialization
libraries. Here's an example you might see using the
[aeson package](/package/aeson):

```haskell
data Person = Person { name :: Text, age :: Int } deriving Show

-- We expect a JSON object, so we fail at any non-Object value.
instance FromJSON Person where
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"
    parseJSON _ = empty
```

To go along with this, we have two helper operators that are less
frequently used:

*   `*>` ignores the value from the first argument. It can be defined as:

    ```haskell
    a1 *> a2 = (id <$ a1) <*> a2
    ```

    Or in `do`-notation:

    ```haskell
    a1 *> a2 = do
        _ <- a1
        a2
    ```

    For `Monad`s, this is completely equivalent to `>>`.

*   `<*` is the same thing in reverse: perform the first action then
    the second, but only take the value from the first action. Again,
    definitions in terms of `<*>` and `do`-notation:

    ```haskell
    (<*) = liftA2 const

    a1 <* a2 = do
        res <- a1
        _ <- a2
        return res
    ```

## Various monadic binding/composition operators

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>) :: Monad m => m a -> m b -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
```

There are a few different monadic binding operators. The two most
basic are `>>=` and `>>`, as they can be trivially expressed in
`do`-notation. And as previously mentioned, `>>` is just a synonym for
`*>` from the `Applicative` class, so it's even easier.

```haskell
m1 >>= f = do
    x <- m1
    f x

m1 >> m2 = do
    _ <- m1
    m2
```

In addition to these two operators, there are also composition
operators for when you have two monadic functions. `>=>` pipes the
result from the left side to the right side, while `<=<` pipes the
result the other way. In other words:

```haskell
f >=> g = \x -> do
    y <- f x
    g y

g <=< f = \x -> do
    y <- f x
    g y

f >=> g = g <=< f
g >=> f = f <=< g
```

## More operators!

If you're aware of other common operators that cause confusion, please
open an issue or a PR to extend this document!
