  $ cat >repro.ml <<'EOF'
  > type t = ( :: )
  > let f (x: t) =  x
  > EOF

FIXME: this should not hang and return a matching.
$ $MERLIN single case-analysis -start 2:16 -end 2:17 \
> -filename repro.ml <repro.ml
