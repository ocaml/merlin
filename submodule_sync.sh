#!/bin/sh
SUBMODULES="vim/vimbufsync"

git submodule update
for i in $SUBMODULES; do
  [ -e "$i".git/.git ] || {
    echo "Uninitialized submodule '$i', skipping (you may need to run 'git submodule init')."
    continue
  }
  echo "Updating '$i'."
  rm -rf "$i"
  cp -R "$i".git "$i"
  rm -rf "$i"/.git
done
