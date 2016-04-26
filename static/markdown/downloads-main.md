
There are a few ways to install a compiler for getting started with Haskell, but the easiest is Stack.

## Stack

### What it is <a name="stack"></a>

Stack is a cross-platform build tool for Haskell that handles management of the toolchain (including the GHC compiler and MSYS2 on Windows), building and registering libraries, and more.

### What you get

- Once downloaded, Stack has the capacity to download and install GHC and other core tools.
- Project development is isolated within sandboxes, including automatic download of the right version of GHC for a given project.
- Stack manages all Haskell-related dependencies, ensuring reproducible builds.
- Stack fetches from a curated repository of over a thousand packages by default, known to be mutually compatible.
- Stack can still generate standalone binaries from your projects just like vanilla GHC, because it uses GHC under the hood.
- Stack can optionally use Docker to isolate the build process and ensure everyone working on a Stack project is using the same environment.

### How to get it

The [install and upgrade page](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
describes how to download Stack on various platforms, although the main
three are repeated here:

- [Ubuntu Linux](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
- [OS X](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#os-x)
- [Windows](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#windows)

Instructions for other Linux distributions, including Debian, Fedora, Red Hat,
Nix OS, and Arch Linux, are also available.

### Where to get help <a name="stackhelp"></a>

For help with Haskell and GHC in general, see the links mentioned
[above](#help). For Stack itself there are also the following resources:

- The [README](https://github.com/commercialhaskell/stack/#readme) offers a
  general overview, and help with installation.
- There is an
  [in-depth guide](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md)
  to using Stack.
- [Getting started with Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
  introduces how to build new projects using Stack.
- You may post issues and feature requests on its
  [GitHub issue tracker](https://github.com/commercialhaskell/stack).
- There is a [mailing list for Stack](https://groups.google.com/d/forum/haskell-stack)
- There is a dedicated
  [\#haskell-stack IRC channel](irc://irc.freenode.net/haskell-stack) on the
  Freenode IRC network.
- The [StackOverflow haskell-stack tag](http://stackoverflow.com/questions/tagged/haskell-stack)
  has many stack-specific questions and answers.
