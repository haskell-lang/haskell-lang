# Downloads

## Package manager

If you are using an operating system which has an up-to-date package
set, you may be able to simply install everything via that.

* [Arch Linux Haskell package guidelines](https://wiki.archlinux.org/index.php/Haskell_Package_Guidelines)

## Haskell Platform

The Haskell Platform is a comprehensive, robust development
environment for programming in Haskell. For new users the platform
makes it trivial to get up and running with a full Haskell development
environment.

For experienced developers, the platform provides a comprehensive,
standard base for commercial and open source Haskell development that
maximises interoperability and stability of your code.

[Download now →](http://www.haskell.org/platform/)

## Advanced setup

Alternatively one can manually install GHC and Cabal packages, but
note, is particularly easier on POSIX (e.g. Linux, BSD, OS X) systems.

### 1. Install GHC

GHC has its own web site with license information, FAQ, download links
and changelogs. Depending on your operating system, there should be a
package made for its package manager, otherwise (e.g. Windows) it will
be an installer.

You can also download the .tar.gz/.zip and unpack and install the
executables and so forth manually.

Or you can even install from source, for which
[there is documentation](https://ghc.haskell.org/trac/ghc/wiki/Building).

[Download GHC now →](https://www.haskell.org/ghc/download)

### 2. Install Cabal-install

After installing GHC, you will want the Haskell package manager:

[Get the Cabal archive →](http://hackage.haskell.org/package/cabal-install)

Download the tar.gz file, extract and inside the resulting directory,
run:

    $ sh ./bootstrap.sh

This will automatically download and install all the packages
necessary to setup Cabal install.

Once complete, you should add `$HOME/.cabal/bin` to your PATH. A
simple way to do this is to edit your `~/.bashrc` and place in there:

    export PATH=$HOME/.cabal/bin:$PATH

Now you should be able to run cabal:

    $ cabal --version
    cabal-install version 1.18.0.2
    using version 1.18.1.2 of the Cabal library

And can start writing Haskell programs and installing packages!
