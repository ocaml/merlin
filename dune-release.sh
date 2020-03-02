#!/bin/sh

TAG="$1"
VER="$2"

if [ -z "$TAG" ]; then
  printf "Usage: ./dune-release.sh <tag-name> [<pkg-version>]\n"
  printf "Please make sure that dune-release is available.\n"
  exit 1
fi

FLAGS="-t $TAG"

if [ -n "$VER" ]; then
  FLAGS="$FLAGS --pkg-version=$VER"
fi

step()
{
  printf "Continue? [Yn] "
  read action
  if [ "x$action" == "xn" ]; then exit 2; fi
  if [ "x$action" == "xN" ]; then exit 2; fi
}

dune-release distrib -p merlin -n merlin $FLAGS --skip-tests #--skip-lint
step
dune-release publish distrib -p merlin -n merlin $FLAGS
step
dune-release opam pkg -p merlin -n merlin $FLAGS
step
dune-release opam submit -p merlin -n merlin $FLAGS
