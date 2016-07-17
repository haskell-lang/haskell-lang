# Exception Safety

This tutorial covers a relatively broad and previously underdocumented
topic in the Haskell ecosystem. In particular, goals of this tutorial
are:

* Explain the different types of exceptions
* Explain how exceptions can be generated
* Explain how interruptible actions work
    * More to the point: interruptible actions introduce a concept of
      trading off between *correctness* and *deadlock prevention*
* Define requirements for proper exception safety in
    * Normal code
    * Resource acquisition code
    * Resource cleanup code
* Give guidelines on how to write exception safe code in general
* Point out some existing violations of these rules in the Haskell
  ecosystem

Many of the ideas behind this tutorial originated with the
[safe-exceptions](/package/safe-exceptions) package, and as such it is
strongly recommended that you work with that package to get maximum
correctness and ease-of-use.

Like the safe-exceptions package, this tutorial takes runtime
exceptions in Haskell as a fact, not a point for discussion, as this
is the reality of the library ecosystem today. No guidelines are given
here for when you yourself should generate runtime exceptions versus
using something like `IO (Either MyError a)` or `ExceptT`. That
discussion is left for a different time.

## Types of exceptions

We're going to follow the safe-exceptions package's definitions for
exception types. It is strongly recommended that you read
[the safe-exceptions tutorials](/package/safe-exceptions) first. As a
quick summary:

* While there is a difference in the type hierarchy for synchronous
  and asynchronous exceptions (via the `SomeAsyncException` type),
  this distinction is not enforced by the `Control.Exception` module
  (e.g., you can synchronously throw via `throwIO` an
  asynchronously-typed exception).
* Despite this limitation, we assume that we can fully distinguish
  whether an exception was thrown synchronously or asynchronously via
  its type. This contract is enforced by the `Control.Exception.Safe`
  module.
* Impure exceptions are exceptions present in pure code, where
  evaluating a pure value results in an exception. These are treated
  fully as synchronous exceptions.

## Defining exception safety

If we're going to strive for something like exception safety, we
better know what exactly we're looking for! Following
[David Abrahams's exception safety guarantees](https://en.wikipedia.org/wiki/Exception_safety),
we're currently looking for level 3 "basic exception safety", aka
"no-leak guarantee", which quoting from Wikipedia means:

> Partial execution of failed operations can cause side effects, but
> all invariants are preserved and there are no resource leaks
> (including memory leaks). Any stored data will contain valid values,
> even if they differ from what they were before the exception.

Two of the most common examples of functions that exist which require exception safety are:

* `withFile`, opens a file, performs an action with the file handle,
  and then closes the file. We need to ensure that the file is always
  closed, regardless of any exceptions thrown, when `withFile` exits
* `withMVar` (or `modifyMVar`), which should ensure that in all cases
  the `MVar` is left filled when the function exits

With the definition and resulting concrete goals in mind, let's dive
into actually writing such safe code.

## Motivating case: `withFile`

Let's take a simple fake implementation of `withFile` to motivate
exception safety. Our first stab at it looks like this:

```haskell
withFile fp inner = do
    h <- openFile fp
    result <- inner h
    closeFile h
    return result
```

This is clearly _not_ exception safe: if the function `inner` throws
an exception, `closeFile` will never be called, and the file will
remain open after exiting `withFile`. This violates our guarantee
described above. So let's throw that out as obviously incorrect code,
and try something slightly better as a baseline:

```haskell
withFile fp inner = do
    h <- openFile fp
    inner h `finally` closeFile h
```

This code looks a lot better, since an exception from `inner` will not
prevent `closeFile` from running. But we're not out of the woods yet:

* How do we deal with asynchronous exceptions?
* What happens if `openFile` throws an exception?
* What happens if `closeFile` throws an exception?
* How do we know which `IO` actions can throw an exception?

All four of these questions will drive some serious exploration of our
design space. Let's attack them one by one.

## Asynchronous exceptions

The basic idea of async exceptions is that they can occur _anywhere_:
an `IO` action, pure code, during a monadic bind, etc. In our
`withFile` example above, an async exception can be thrown:

* Before the call to `openFile`
* During the call to `openFile`
* Between `openFile` and `finally`
* After `finally` before exiting `withFile`
* Inside `inner`

As an exercise, I recommend that you step through these different
cases and see whether our `withFile` leaks resources or not.

The first trick we need to add to our arsenal is *masking*. This sets
up a region of code wherein asynchronous exceptions are guaranteed
_not_ to be thrown. Usage is pretty straightforward:

```haskell
withFile fp inner = mask $ \restore -> do
    h <- openFile fp
    restore (inner h) `finally` closeFile h
```

The `restore` function will restore the original masking state, which
usually means "turn async exceptions back on." We restore async
exceptions for the inner action so as to avoid creating an
impossible-to-kill thread, but make sure that we do all of our
allocation and cleanup in a masked state so we can avoid getting
exceptions at unexpected places. We now know for certain that only
`IO` actions have the potential to throw an exception, and ensure that
our code does not leak in the case of any of those actions leaking
exceptions.

__Exercise__ Write a function called `with2Files` which will open and
guarantee the close of two files instead of just one. You can first
implement in terms of `withFile`, and then implement without the help
of `withFile`.

__Solution__

```haskell
with2Files fp1 fp2 inner = mask $ \restore -> do
    h1 <- openFile fp1
    h2 <- openFile fp2 `onException` closeFile h1
    restore (inner h1 h2) `finally` closeFile h2 `finally` closeFile h1
```

The big takeaway you should get from this exercise: while masking
prevent asynchronous exceptions from running, it does _nothing_ to
prevent synchronous exceptions. You must still deal with those
explicitly.

## Exceptions in an allocation function

It's entirely possible that the `openFile` function itself may throw
an exception (e.g., permission denied to open the given file). If we
think at all about our defined guarantees above, the behavior of
`openFile` in that situation should be obvious: it must not leak any
resources! We must be guaranteed that, if an exception is thrown in
`openFile`, it cleans up after itself completely.

Using our example of `with2Files`, we can demonstrate the proper way
to write an `open2Files` function that can be used for allocation:

```haskell
open2Files fp1 fp2 = do
    h1 <- openFile fp1
    h2 <- openFile fp2 `onException` closeFile h1
    return (h1, h2)
```

Notice how `h1` will be closed if `fp2` throws an exception, and only
on success will both handles remain open. This is the behavior desired
of a well written allocation function.

The other thing to point out is that _we don't mask exceptions_. It is
_not_ the responsibility of the allocation function to mask
asynchronous exceptions, it can rely on the fact that the caller has
done so already. The logic behind this is: there is no way for the
allocation function to mask exceptions outside of itself, and doing so
is necessary for proper exception safety. Since the allocator has no
power to do this itself, it demands masking as a precondition to being
called.

## Interruptible actions

There's an extra aspect to exceptions which is somewhat orthogonal to the sync/async breakdown, but is integral to proper exception safety. Some `IO` actions are _interruptible_. 

## FIXME

Write up a tutorial on bracket, interruptible exceptions, etc. Why
hClose is broken. Trade-off between deadlock from incorrect code and
writing incorrect code by mistake. uninterruptible mask is probably
the right default for many cases
