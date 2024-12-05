  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

  $ mkdir real_source

  $ cat >real_source/pparse.ml <<'EOF'
  > let x = "Hello"
  > EOF

  $ mkdir source

  $ cat >source/main.ml <<'EOF'
  > let () = print_endline Pparse.x 
  > EOF

  $ cat >source/dune <<'EOF'
  > (rule (copy# ../real_source/pparse.ml pparse.ml))
  > (executable (name main))
  > EOF


  $ dune build @check
  $ dune exec ./source/main.exe
  Hello

  $ $MERLIN single locate -look-for ml -position 1:30 \
  > -filename source/main.ml <source/main.ml
  {
    "class": "return",
    "value": "'Dune__exe.Pparse.x' seems to originate from 'Pparse' whose ML file could not be found",
    "notifications": []
  }
