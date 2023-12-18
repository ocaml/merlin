  $ $MERLIN server stop-server

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >dune <<EOF
  > 
  > (executable
  >  (name main)
  >  (modules main)
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_int 0
  > EOF

Let's populate file cache
  $ $MERLIN server errors -log-file merlin_logs -cache-lifespan 45 \
  > -filename main.ml 1> /dev/null <main.ml

When cache time is set to large value, we keep the cache
  $ $MERLIN server errors -log-file merlin_logs -cache-lifespan 45 \
  > -filename main.ml 1> /dev/null <main.ml
  $ cat merlin_logs | grep -A1 "File_cache(Cmi_cache) - flush" \
  > | tail -1 | sed 's/\ ".*\"//'
  keeping

When cache time is set to 0, file cache gets flushed
  $ $MERLIN server errors -log-file merlin_logs -cache-lifespan 0 \
  > -filename main.ml 1> /dev/null <main.ml
  $ cat merlin_logs | grep -A1 "File_cache(Cmi_cache) - flush" \
  > | tail -1 | sed 's/\ ".*\"//'
  removing

Stop server
  $ $MERLIN server stop-server
