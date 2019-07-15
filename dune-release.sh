#!/bin/sh

TAG="$1"

if [ -z "$TAG" ]; then
  printf "Usage: ./release.sh <tag-name>\n"
  printf "Please make sure that dune-release is available.\n"
  exit 1
fi

step()
{
  printf "Continue? [Yn] "
  read action
  if [ "x$action" == "xn" ]; then exit 2; fi
  if [ "x$action" == "xN" ]; then exit 2; fi
}

dune-release distrib -p merlin -n merlin -t "$TAG" --skip-tests --skip-lint
step
dune-release publish distrib --change-log=CHANGELOG.md -p merlin -n merlin -t "$TAG"
step
dune-release opam pkg --change-log=CHANGELOG.md -p merlin -n merlin -t "$TAG"
step
dune-release opam submit --change-log=CHANGELOG.md -p merlin -n merlin -t "$TAG"
