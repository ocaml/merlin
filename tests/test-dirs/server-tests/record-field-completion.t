Make sure that this test doesn't depend on previous state
  $ $MERLIN server stop-server

  $ cat > a.ml <<EOF
  > module M = struct
  >   type t =
  >     { ababagalamaga1: int
  >     ; ababagalamaga2: int
  >     }
  > end 
  > 
  > let f (t: M.t) = t.
  > EOF

  $ cat > .merlin <<EOF
  > USE_PPX_CACHE
  > EOF


The inlay-hints command will populate the reader cache, but not set for_completion position
  $ $MERLIN server inlay-hints -start start -end end -filename a.ml -log-file merlin_logs 1> /dev/null <a.ml


The complete-prefix command returns the field names
  $ $MERLIN server complete-prefix -position end -log-file merlin_logs -filename a.ml <a.ml | grep 'ababagalamaga' | wc -l
  2

The reader cache was invalidated from previous non-for-completetion command
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

Stop server
  $ $MERLIN server stop-server
