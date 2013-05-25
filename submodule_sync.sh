#!/bin/sh
git submodule sync
rm -rf vim/vimbufsync
cp -R vim/vimbufsync.git vim/vimbufsync
rm -rf vim/vimbufsync/.git
