There are three widely used ways to install the Haskell toolchain on supported
platforms. These are:

- [Minimal installers](#minimal): Just GHC (the compiler) and Cabal (a package
  install and build tool) are installed globally on your system, using your
  system's package manager.

- [Stack](#stack): Installs the `stack` command globally: a project-centric
  build tool to automatically download and manage Haskell dependencies on a
  project-by-project basis.

- [Haskell Platform](#platform): Installs GHC, Cabal, and some other tools,
  along with a starter set of libraries in a global location on your system.

These options make different choices as to what is installed globally on your
system and what is maintained in project-specific environments. Global
installations allow more sharing across users and projects, but at the cost of
potential conflicts between projects. To avoid these conflicts, each option
has a lightweight *sandboxing* feature that creates largely self-contained,
per-project environments. With Minimal you can optionally sandbox the
libraries, avoiding most conflicts. Stack sandboxes the compiler, tools and
libraries, so avoids nearly all kinds of conflicts between projects. With
Platform you can also optionally sandbox libraries, but not the globally
installed platform libraries.

<!-- For information on other platforms and methods, please see the section on
[third party installers](#other). -->

<hr style="height: 1px; background-color: black;" --/>

## Minimal installers

### What they are <a name="minimal"></a>

Minimal installers provide only the
[GHC](https://www.haskell.org/ghc)  compiler and the
[Cabal](https://www.haskell.org/cabal/) and
[Stack](https://github.com/commercialhaskell/stack) tools for installing packages.

### What you get

- Only the core libraries necessary for each platform are included.
- Cabal or Stack must be used to download and install packages after installation.

### How to get them

- [Linux](/downloads/linux)
- [OS X](https://ghcformacosx.github.io/) (via GHC for Mac OS X)
- [Windows](https://github.com/fpco/minghc#using-the-installer) (via MinGHC)

### Where to get help <a name="help" />

- For help learning Haskell itself, start with the  [Documentation](https://www.haskell.org/documentation) page on the [Haskell Wiki](https://wiki.haskell.org/).
- If you need help with [GHC](https://www.haskell.org/ghc)---the Haskell
  compiler---there is a comprehensive
  [GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html).
- For help using Cabal to download or create additional packages (see
  [below](#libraries)), there is the
  [Cabal User Guide](https://www.haskell.org/cabal/users-guide/).
- For help using Stack to download or create packages, see the stack documentation [below](#stackhelp).
- Finally, you can ask questions of other Haskell users and experts on the
  [\#haskell IRC channel](irc://irc.freenode.net/haskell) on the Freenode IRC
  network.

## Stack

### What it is <a name="stack"></a>

Stack is a cross-platform build tool for Haskell that
handles management of the toolchain (including the GHC compiler and MSYS2 on
Windows), building and registering libraries, and more.

### What you get

- Once downloaded, it has the capacity to download and install GHC and other
  core tools.
- Project development is isolated within sandboxes, including automatic
  download of the right version of GHC for a given project.
- It manages all Haskell-related dependencies, ensuring reproducible builds.
- It fetches from a curated repository of over a thousand packages by default,
  known to be mutually compatible.
- It can optionally use Docker to produce standalone deployments.

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

## Haskell Platform

### What it is

<a name="platform"></a>The Haskell Platform is a self-contained, all-in-one
installer. After download, you will have everything necessary to build Haskell
programs against a core set of useful libraries.

### What you get

- The [Glasgow Haskell Compiler](https://www.haskell.org/ghc)
- The [Cabal build system](https://www.haskell.org/cabal/), which can install
  new packages, and by default fetches from
  [Hackage](https://hackage.haskell.org/), the central Haskell package
  repository.
- Support for profiling and code coverage analysis
- 35 core & widely-used [packages](https://www.haskell.org/platform/contents.html)

### How to get it

The Platform is provided as a single installer, and can be downloaded at the
links below.

- [Linux](http://www.haskell.org/platform/linux.html)
- [OS X](http://www.haskell.org/platform/mac.html)
- [Windows](http://www.haskell.org/platform/windows.html)

### Where to get help

- You can find a comprehensive list of
  [what the Platform offers](https://www.haskell.org/platform/contents.html).
- See the general help mentioned [above](#help), which covers the usage of GHC,
as well as the Cabal and Stack tools.

<hr style="height: 1px; background-color: black;" --/>

## Additional Libraries <a name='libraries'></a>

In Haskell, packages are configured and built with the Cabal package system built into GHC (and other compilers). For more specific details, see [The Cabal User Guide](https://www.haskell.org/cabal/users-guide/). The command line tools to download and install packages are either `cabal` or `stack`, each having different workflows. For details on their usage, see the documentation above.

### Hackage

Hackage is a repository of packages to which anyone can freely upload at any time. The packages are available immediately and documentation will be generated and hosted there. It can be used by cabal install.

You can install a package using cabal by running:

    $ cabal update
    $ cabal install the-package

Note that if you are not in a sandbox, this will install the package globally, which is often not what you want, so it is recommended to set up sandboxes in your project directory by running `cabal sandbox init`.

[Go to Hackage →](https://hackage.haskell.org/packages/)

### LTS Haskell

LTS Haskell is a stackage-based long-term support set of packages which build and pass tests together, with backported bug fixes.

[Get LTS Haskell →](http://www.stackage.org/lts)

### Stackage Nightly

Stackage is a nightly generated stable repository of snapshots of package sets in which only packages which build and pass tests together are bundled together into a snapshot.

[Get Stackage Nightly →](http://www.stackage.org/nightly-2015-09-29)

### From source control repositories

Installing from a source repository is also possible. For example, to clone and install the network package from source, you would run:

    $ git clone git@github.com:haskell/network.git
    $ cabal install network/

Or:

    $ git clone git@github.com:haskell/network.git
    $ cd network
    $ cabal install
