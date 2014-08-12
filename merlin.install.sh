#!/bin/sh
(
  printf 'bin: [\n'
  printf '  "ocamlmerlin"\n'
  printf '  "omake-merlin"\n'
  printf '  "jenga-merlin"\n'
  printf ']\n'
  printf '\n'
  printf 'share: [\n'
  find vim/merlin/ vim/vimbufsync/ -type f -a \
    \! \( -name '.*' -o -name '*.pyc' -o -name 'README.md' \) \
    -printf '  "%p" { "vim/%P" }\n'
  printf '  "emacs/merlin.el" {"../emacs/site-lisp/merlin.el"}\n'
  printf '  "?emacs/merlin.elc" {"../emacs/site-lisp/merlin.elc"}\n'
  printf ']\n'
) > merlin.install
