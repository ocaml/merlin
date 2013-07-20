#!/bin/sh
D=`mktemp -d`
cleanup()
{
  rm -rf "$D"
}

trap cleanup EXIT
OUT="$D/omake.output"
mkfifo "$OUT"
exec "$@" 3>&1 1>&2 2>&3 | tee "$OUT" 3>&1 1>&2 2>&3 &
PID=$!

grep --line-buffered "^*** omake: polling for filesystem changes$" "$OUT" |
while read line; do
  echo "*** merlin: reloading cache"
  killall --user "$USER" --signal SIGUSR1 --regexp '^ocamlmerlin.*'
done
wait "$PID"
