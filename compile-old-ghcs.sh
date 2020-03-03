#!/bin/bash

update() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.0 cabal update
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.2 cabal new-update
}

ghc80() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.0 cabal install --dep
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.0 cabal configure
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.0 cabal build
}

ghc82() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.2 cabal new-build
}

ghc84() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.4 cabal v2-build
}

ghc86() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.6 cabal v2-build
}

main() {
  update
  ghc80 &
  ghc82 &
  ghc84 &
  ghc86 &
  wait
  echo 'This script created a docker volume "tmphs". You may be interested in removing it' >&2
}

main "$@"
