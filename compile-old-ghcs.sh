#!/bin/bash

update() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.4 cabal v2-update
}

ghc84() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.4 cabal v2-build
}

ghc86() {
  docker run --rm -i -w "$PWD" -v "$PWD:$PWD" -v tmphs:/root haskell:8.6 cabal v2-build
}

main() {
  set -e
  trap 'echo This script created a docker volume "tmphs". You may be interested in removing it' exit
  update
  ghc84
  ghc86
}

main "$@"
