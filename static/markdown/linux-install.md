## Package-based install

### Ubuntu

Steps to setup:

    $ sudo apt-get update
    $ sudo apt-get install python-software-properties
    $ sudo add-apt-repository -y ppa:hvr/ghc
    $ sudo apt-get update
    $ sudo apt-get install cabal-install-1.20 ghc-7.8.2 happy-1.19.3 alex-3.1.3

Add

    ~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.2/bin:/opt/happy/1.19.3/bin:/opt/alex/3.1.3/bin

to your `PATH`. You can typically do this if your shell is Bash by adding:

    export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.2/bin:/opt/happy/1.19.3/bin:/opt/alex/3.1.3/bin:$PATH

To your `~/.bashrc` file. If you use another shell (e.g. zsh), check the
manual for how to set environment variables.

### Arch Linux

To install Haskell from the official repos on Arch Linux:

Update your mirrorlist:

    sudo pacman -Syy

Download and install Haskell:

    sudo pacman -S cabal-install ghc happy alex haddock
