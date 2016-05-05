Quick steps to get up and running with Haskell quickly!

## <span class="counter">1</span> Download Haskell Stack

<p class="os-logos">
<a class="os-logo" href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#mac-os-x">
<img src="/static/img/apple-logo.svg">
</a>

<a class="os-logo" href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows">
<img src="/static/img/windows-logo.svg">
</a>

<a class="os-logo" href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#linux">
<img src="/static/img/linux-logo.svg">
</a>
</p>

[Other operating systems](http://docs.haskellstack.org/en/stable/install_and_upgrade)

Included goodies:

* **Stack**: A project builder for multi-package Haskell projects.
* **GHC**: A compiler and interpreter for Haskell programs.
* **Haddock**: A documentation generator for Haskell packages.
* **Hoogle**: A search tool for searching Haskell packages.
* And thousands of packages installed on demand.

## <span class="counter">2</span> Running Haskell scripts

To quickly run a Haskell script:

1.  Copy the following content into a file called `HelloWorld.hs`:

    ```haskell
    #!/usr/bin/env stack
    -- stack --install-ghc runghc

    main :: IO ()
    main = putStrLn "Hello World"
    ```

2. Open up a terminal and run `stack HelloWorld.hs`.

Done!

## <span class="counter">3</span> Write your own Haskell package

This is a good way to start on a proper Haskell package.

```
stack new new-project
cd new-project
stack build
stack exec new-project-exe
```

You can now edit the source files in this directory (see the file
`src/Lib.hs`), and run the project with `stack exec new-project-exe`
as above.

## <span class="counter">4</span> Next steps

Congratulations, you're ready to start writing Haskell code! Now you're ready to:

* [Learn about Haskell the language](/documentation)
* [Write Haskell projects with Stack](http://docs.haskellstack.org/en/stable/GUIDE/)
* [Browse packages that you can use in your projects](/packages)
