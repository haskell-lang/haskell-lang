Get up and running with Haskell development quickly!

## Step 1: Download Stack

[Stack](http://haskellstack.com) is the build tool of choice for Haskell.

* [View the full Stack installation page](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
* Instructions for
  [Windows](http://docs.haskellstack.org/en/stable/install_and_upgrade#windows),
  [Mac OS X](http://docs.haskellstack.org/en/stable/install_and_upgrade#mac-os-x),
  [Ubuntu](http://docs.haskellstack.org/en/stable/install_and_upgrade#ubuntu),
  [Debian](http://docs.haskellstack.org/en/stable/install_and_upgrade#debian),
  [CentOS / Red Hat / Amazon Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade#centos),
  [Fedora](http://docs.haskellstack.org/en/stable/install_and_upgrade#fedora),
  [openSUSE / SUSE Linux Enterprise](http://docs.haskellstack.org/en/stable/install_and_upgrade#suse),
  [Arch Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade#arch-linux),
  [NixOS](http://docs.haskellstack.org/en/stable/install_and_upgrade#nixos),
  [Linux (general)](http://docs.haskellstack.org/en/stable/install_and_upgrade#linux),
  [FreeBSD (unofficial)](http://docs.haskellstack.org/en/stable/install_and_upgrade#freebsd)

Stack will be responsible for downloading the correct version of
[GHC](http://www.haskell.org/ghc) and installing packages.

## Step 2: Run Hello World

1.  Copy the following content into HelloWorld.hs:

    ```haskell
    #!/usr/bin/env stack
    -- stack --install-ghc runghc

    main :: IO ()
    main = putStrLn "Hello World"
    ```

2.  Run `stack HelloWorld.hs`.  This will download and setup GHC in a local
    directory and then run this program.

    * If you get errors about `stack` not being found, please review the
      installation instructions above.

    * If you get an error message about `HelloWorld.hs` not being found, please
      ensure you run the command from the directory containing the file.

## Step 3 (optional): Start a project

This is a good way to kick the tires on a bigger project.

```
stack new new-project
cd new-project
stack build --file-watch
```

You can now edit the source files in this directory, and Stack will
automatically rebuild each time you make a change.

## Step 4: Learn more

Congratulations, you're ready to start writing Haskell code! Now you're ready to:

* [Read docs on this website](/documentation)
* [Join the community](/community)
* Browse [the LTS Haskell package set](https://www.stackage.org/lts) to see
  what packages are available and read their API docs

To learn more about Stack:

* Read [the Stack guide](http://docs.haskellstack.org/en/stable/GUIDE/) to learn all about Stack
* [Getting started with Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
  introduces how to build new projects using Stack.
* There is a [mailing list for Stack](https://groups.google.com/d/forum/haskell-stack)
* There is a dedicated
  [\#haskell-stack IRC channel](irc://irc.freenode.net/haskell-stack) on the
  Freenode IRC network.
* The [StackOverflow haskell-stack tag](http://stackoverflow.com/questions/tagged/haskell-stack)
  has many stack-specific questions and answers.
