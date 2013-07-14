#!/bin/sh
D=`mktemp -d`
cleanup()
{
  rm -rf "$D"
}

trap cleanup EXIT
OUT="$D/omake.output"
mkfifo "$OUT"
exec "$@" | tee - "$OUT" &
PID=$!

grep "^*** omake: polling for filesystem changes$" "$OUT" |
while read line; do
  killall --user "$USER" --signal SIGUSR1 --regexp '^ocamlmerlin.*'
done
wait "$PID"
