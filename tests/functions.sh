#!/bin/sh

cmd()
{
  printf '["%s"' "$1"
  shift 1
  for arg in "$@"; do
    if [[ "$arg" =~ ^[A-Za-z] ]]; then
      printf ',"%s"' "$arg"
    elif [ -z "$arg" ]; then
      printf ',""'
    else
      printf ',%s' "$arg"
    fi
  done
  printf ']\n'
}

reset()
{
  FILENAME="${1:-test.ml}"
  cmd reset auto "$FILENAME"
}

json_escape()
{
  printf '"'
  while IFS= read INPUT; do
    INPUT=${INPUT//\\/\\\\}
    INPUT=${INPUT//\//\\\/}
    #INPUT=${INPUT//\'/\\\'}
    INPUT=${INPUT//\"/\\\"}
    printf '%s\\n' "$INPUT"
  done
  printf '"'
}


tell-more()
{
  json_escape | { read -r msg; cmd tell source "$msg"; }
}

tell()
{
  cmd tell start
  tell-more
  cmd tell eof
}

package()
{
  cmd find use "$@"
}

extension()
{
  cmd extension enable "[\"$1\"]"
}

pos()
{
  printf '{"line":%d,"col":%d}' "$1" "$2"
}
