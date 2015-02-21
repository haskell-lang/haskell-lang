## Package-based install

### Ubuntu

Steps to setup:

    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install-1.20 ghc-7.8.4
    cat >> ~/.bashrc <<EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH
    cabal update
    cabal install alex happy

### Fedora 21

To install GHC 7.8.4 from the unofficial repo (Fedora 22+ will include it in the official one):

    sudo yum-config-manager --add-repo https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/repo/fedora-21/petersen-ghc-7.8.4-fedora-21.repo
    sudo yum install ghc cabal-install

As stated in [petersen/ghc-7.8.4 copr page](https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/) this ghc cannot be installed in parallel with Fedora/EPEL ghc.

If you want to install from the official repo that uses an older version of ghc (7.6.x) and cabal-install (1.16.x), just run the install command without adding the unofficial repo.

### Arch Linux

The official repos on Arch Linux (extra, specifically) contain packages `ghc`, `cabal-install`, `happy`, `alex`, `haddock`. To install them,

1) Update the mirrorlist and the system itself

    sudo pacman -Syu

2) Download and install:

    sudo pacman -S ghc cabal-install happy alex haddock

Of course you can choose to omit the system upgrade and simply do

    sudo pacman -Sy ghc cabal-install happy alex haddock
    
As a responsible arch user you know the inherent risks, i am certain.
