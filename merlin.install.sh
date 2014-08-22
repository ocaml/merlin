#!/bin/sh
(
  printf 'bin: [\n'
  printf '  "ocamlmerlin"\n'
  printf ']\n'
  printf '\n'
  printf 'share: [\n'
  find vim/merlin vim/vimbufsync -type f -a \
    \! \( -name '.*' -o -name '*.pyc' -o -name 'README.md' \) |
  sed -e 's%^[^/]*/[^/]*/\(.*\)%  "&" { "vim/\1" }%' | sort -u
  printf '  "emacs/merlin.el" {"../emacs/site-lisp/merlin.el"}\n'
  printf '  "?emacs/merlin.elc" {"../emacs/site-lisp/merlin.elc"}\n'
  printf ']\n'
) > merlin.install
