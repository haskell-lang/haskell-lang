# How to Play with Stack

This tutorial assumes you've already installed the Stack build
tool. If you haven't yet, please start with the
[get started page](/get-started) and then come back here.

You may also be interested in learning:

* [How to Script with Stack](stack-script)
* [How to Build with Stack](stack-build)

In this article we'll be talking about how to play with Haskell by
using the REPL and building simple programs.

## Installing GHC

The next thing we might want to do is install the GHC compiler. We can use stack to install
it for us. To do that, all we need to write is:

```
suppi@ubuntu:~$ stack setup
Run from outside a project, using implicit global project config
Using latest snapshot resolver: lts-6.14
[snip]
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
```

After that, the you'll be able to run `ghc`, `runghc` and `ghci` anywhere by typing

```
stack exec -- <command>
```

For example:

```
suppi@ubuntu:~$ stack exec -- ghc --version
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
The Glorious Glasgow Haskell Compilation System, version 7.10.3
```

## Playing with Haskell

Let's try opening `ghci` and write something

```
suppi@ubuntu:~$ stack exec -- ghci
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> putStrLn "Hello, Haskell!"
Hello, Haskell!
Prelude> :q
Leaving GHCi.
```

Ok! so Stack ran GHCi using the global project configuration.
Stack can manage different dependencies for different projects, so when you are
not in a folder associated with a project, it will use the default configuration.

Next, we write some haskell code: `putStrLn "Hello, Haskell!"` and GHCi immediately
evaluate and execute it.

After that, we type the `:q` command to leave GHCi.


Here's an example of using `ghc` to compile a Haskell source file
and then running the compiled executable:

```
suppi@ubuntu:~$ cat > Main.hs
module Main where

main :: IO ()
main = putStrLn "Hello from Haskell!"
^C
suppi@ubuntu:~$ stack exec -- ghc Main.hs -o HiHaskell
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking HiHaskell ...
suppi@ubuntu:~$ ./HiHaskell 
Hello from Haskell!
```

We can also just run the file without compiling it using `runghc`:

```
suppi@ubuntu:~$ stack exec -- runghc Main.hs
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
Hello from Haskell!
```

By default, Stack will only use the standard Haskell library called `base`.
If we'd like to use external packages, for example: `http-client`, we can use
the `--package` flag. This flag will download the package specified and its dependencies
the first time, so it may take some time to load, but the next time you will try it,
Stack will use the packages it downloaded the first time.

Here's a short example of running a `GET` request using http-client from GHCi:

```
suppi@ubuntu:~$ stack exec --package http-client -- ghci
Run from outside a project, using implicit global project config
Using resolver: lts-6.14 from implicit global project's config file: 
/home/suppi/.stack/global-project/stack.yaml
base64-bytestring-1.0.0.1: download
base64-bytestring-1.0.0.1: configure
[snip]
http-client-0.4.31: build
http-client-0.4.31: copy/register
Completed 25 action(s).
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> import Network.HTTP.Client
Prelude Network.HTTP.Client> manager <- newManager defaultManagerSettings
Prelude Network.HTTP.Client> request <- parseRequest "http://httpbin.org/get"
Prelude Network.HTTP.Client> response <- httpLbs request manager
Prelude Network.HTTP.Client> putStrLn (read (show (responseBody response)))
{
  "args": {}, 
  "headers": {
    "Accept-Encoding": "gzip", 
    "Host": "httpbin.org"
  }, 
  "origin": "5.102.241.151", 
  "url": "http://httpbin.org/get"
}
```

## What's Next

That's it for this guide. There is of course more to learn, but this
should be enough to start learning from different Haskell learning materials
using Stack.

What's next?

1. [Pickup a learning resource](https://haskell-lang.org/documentation). I can recommend [Haskell Programming from First Principles](http://haskellbook.com), but there are more resources available.

2. [Read the Stack documentation](https://haskellstack.org) to learn more about building and managing Haskell programs and libraries using Stack.

3. [Another, more thorough guide to getting started using Stack](http://howistart.org/posts/haskell/1)

4. [How to write great documentation for Haskell code](http://yannesposito.com/Scratch/en/blog/Haskell-Tutorials--a-tutorial/)

5. [An example for a non-trivial Haskell project](https://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees.html)

6. [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/) - A really long reference for a lot Haskell related concepts, features and libraries. Also links to other materials by subject


Good luck learning Haskell! I hope you'll enjoy it!
