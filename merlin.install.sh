#!/bin/sh
(
  printf 'bin: [\n'
  printf '  "ocamlmerlin"\n'
  printf ']\n'
  printf '\n'
  printf 'share: [\n'
  find vim/merlin -type f -a \
    \! \( -name '.*' -o -name '*.pyc' -o -name 'README.md' \) |
  sed -e 's%^[^/]*/[^/]*/\(.*\)%  "&" { "vim/\1" }%' | sort -u
  printf ']\n'
  printf 'share_root: [\n'
  for i in $(cd emacs/ && echo *.el); do
    printf '  "emacs/%s" {"emacs/site-lisp/%s"}\n' "$i" "$i"
    printf '  "?emacs/%sc" {"emacs/site-lisp/%sc"}\n' "$i" "$i"
  done
  printf ']\n'
) > merlin.install
