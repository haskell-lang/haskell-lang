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

Before we can jump into cleanup actions, we need to take a slight
detour and discuss interruptible actions. These are
[described in the Control.Exception docs](https://www.stackage.org/haddock/nightly-2016-07-17/base-4.9.0.0/Control-Exception.html#g:13). The
basic idea is that these are actions which, even when async exceptions
are masked, can throw an asynchronous exception. These doesn't violate
any of our comments above, since the exceptions are being thrown from
an `IO` action. However, the exceptions are being generated as a kill
signal from outside of our current subroutine, and therefore should be
treated as asynchronous/unrecoverable exceptions.

It's important to understand that the purpose of allowing for such a
concept as an interruptible action is to *avoid deadlocks*. However,
the tradeoff is that interruptible actions make it more difficult to
write exception safe code, since they have allowed yet another
"wormhole" through which exceptions can tunnel into our code. The
simplest approach to dealing with this is: assume all `IO` actions may
throw an exception. In practice, there is a subset of non-blocking
`IO` actions which are known not to be interruptible, and are listed
in the `Control.Exception` documentation linked to above.

Even these interrupting exceptions can be masked as well, using the
`uninterruptibleMask` function. Inside an `uninterrupibleMask`, it's
still entirely possible for a synchronous exception to be generated
(via `throwIO` or equivalent), but no asynchronous exceptions will be
thrown. This is important as we dive into cleanup functions.

## Cleanup functions

Similar to allocation functions, cleanup functions need guarantees
that an asynchronous exception will not be thrown before it starts
executing, and therefore it's necessary for a function like `bracket`
or `withFile` to mask async exceptions. And therefore, just like
allocation functions, we don't need to use `mask` inside our cleanup
functions, since it can be assumed to already be in place.

However, it's less clear that
uninterruptible masking is the right thing. There is a long discussion
of this
[on the safe-exceptions issue tracker](https://github.com/fpco/safe-exceptions/issues/3). Ultimately,
the decision from that disucssion is that it is better to err on the
side of caution and use uninterruptible masking, at the possible risk
of introducing delays or deadlocks. (See the section on `hClose` and
flushing for more details.)

Continuing with our ongoing example, let's look at how to write an
allocation function that closes two files:

```haskell
close2Files (h1, h2) = closeFile h1 `finally` closeFile h2
```

Notice how, even if `closeFile h1` throws an exception, `closeFile h2`
will still be called. This meets our guarantee of ensuring all
resources are cleaned up even in the case of exceptions. This relies
on `closeFile` itself providing the appropriate cleanup guarantees,
namely that when called in a masked state, it guarantees that it will
not return until the resources allocated by `openFile` are freed.

Also, since a cleanup handler is run in such a deadlock-friendly mode
(uninterruptible masking), it should avoid performing long-running
actions whenever possible.

## Difference between allocation and cleanup

We have the following differences between allocation and cleanup
functions:

* Allocations run in a `mask`, cleanups in an `uninterruptibleMask`
* There's no problem with allocations performing long-running,
  blocking actions

These two points go hand-in-hand. An allocation function only has
responsibility to clean up after itself, not after some other action's
resources. It is free to take as much time as necessary to acquire a
resource (like a network connection), as long as it guarantees that,
in the case of failure, it cleans up after itself. And to make that
work, it should _not_ use an `uninterrupibleMask`, in order to avoid
the possibility of undue delays and deadlocks.

By contrast, a cleanup function needs to ensure that resources
acquired elsewhere are always freed. Those cleanups need to happen
regardless of how the main action exited (success, sync exception, or
async exception), and therefore uninterruptible masking is desired to
prevent accidental resource leaks. Since it is a standard mode of
operation that an async exception brought about a cleanup function,
the cleanup needs to assume that blocking behavior is not desired.

## Summary of guarantees

Putting it all together, here are the expected guarantees for our
functions:

* An allocation function:
    * Can assume that it is being called in a masked (interruptible) state
    * Must either successfully allocate a set of resources (file
      descriptor, mutex, etc) or throw an exception and allocate no
      resources at all
* A cleanup function:
    * Can assume it is being called in a masked (uninterruptible)
      state, though for stricter conformance with `Control.Exception`
      should assume that it is called in an interruptible state
    * Must successfully release all resources which were allocated by
      the allocation function
    * Should avoid long-running or blocking actions

With these guarantees met, we can use any given allocation/cleanup
function combination in a `bracket` call and know that, when the
`bracket` call exits, no resources will be leaked.

## `hClose` and flushing

To demonstrate a more complicated corner case, let's consider the
real-life `openFile`/`hClose` allocation/cleanup combo. The actual
`withFile` function from `System.IO` is defined as:

```haskell
withFile name mode = bracket (openFile name mode) hClose
```

The `openFile` function has to set up some bookkeeping variables for
the `Handle`, get a file descriptor, and return. It's relatively
uninteresting. `hClose`, on the other hand, has two distinct
requirements:

1. Flush the remaining data in the buffer to the file descriptor
2. Close the file descriptor

Flushing the buffer is necessary during normal exit (without
exceptions), and is probably desired even in exceptional
cases. However, it's problematic in the case of our thread dying from
an async exception, since it can cause a significant delay in exit
(violating our "Should avoid long-running or blocking actions"
guarantee).

By contrast, the "close the file descriptor" action is absolutely
required for proper safe exception guarantees, regardless of why we're
exiting (normal exit, sync exception, or async exception). Based on
our analysis, there is no way to define `bracket` and `hClose` to make
`withFile` behave exactly the way described here. Instead, we need to
separate out the required and blocking actions from each
other. Assuming that we have `hFlush` and `hCloseWithoutFlush`, this
would look something like:

```haskell
withFile name mode inner = mask $ \restore -> do
    h <- openFile name mode
    eres <- try $ restore $ do
        eres <- try (inner h)
        case eres of
            Left (e :: SomeException)
                -- when an async exception was thrown,
                -- don't block in order to flush
                | isAsyncException e -> return ()
            _ -> hFlush h
        either throwIO return eres

    _ :: Either SomeException () <- try $ uninterruptibleMask_ $ hCloseWithoutFlush h
    either throwIO return eres
```

This hopefully demonstrates the inherent complexity with attempting to
give full exception guarantees while simultaenously avoiding blocking
calls. This is an extreme case which most people will not encounter in
their own code.

Overall, our recommendation is: if you have to choose between a
potential deadlock or a potential resource leak, err on the side of
deadlocking. It will be a more explicit bug, and is typically only
triggered by another mistake in your code. Based on that, if you're
uncertain if you should use `mask` or `uninterruptibleMask`, use the
latter.
