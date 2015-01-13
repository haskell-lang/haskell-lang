## Package-based install

### Ubuntu

Steps to setup:

    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install-1.20 ghc-7.8.3
    cat >> ~/.bashrc <<EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:$PATH
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:$PATH
    cabal update
    cabal install alex happy

### Arch Linux

To install Haskell from the official repos on Arch Linux:

Update your mirrorlist:

    sudo pacman -Syy

Download and install Haskell:

    sudo pacman -S cabal-install ghc happy alex haddock
