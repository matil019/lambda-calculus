# Lambda Calculus and Genetic Algorithm in Haskell

This is an implementation of untyped and simply-typed [lambda calculus][wikip-lc] in Haskell, combined with [genetic algorithm][wikip-ga]. Both are written from scratch, just for fun!

[wikip-lc]: https://en.wikipedia.org/wiki/Lambda_calculus
[wikip-ga]: https://en.wikipedia.org/wiki/Genetic_algorithm

You need `cabal`, the Haskell's build system to build and run the program. If you are new to Haskell, it's a good idea to install it by [Haskell Platform][hs-plt], or by `choco install cabal ghc` (with [Chocolatey][choco], for Windows).

[hs-plt]: https://www.haskell.org/platform/
[choco]: https://chocolatey.org/

Currently, the `main` function in [src/Main.hs](src/Main.hs) tries to find a "plus" function that adds two Church-encoded natural numbers.

Run the program without command line and stop the program with <kbd>Ctrl</kbd>+<kbd>C</kbd>.
