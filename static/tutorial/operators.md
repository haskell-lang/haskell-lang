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
operator (which, in case you're wondering, it "list append"). This
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

## Monoidal append `<>`

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

__FIXME ADD MORE OPERATORS__
