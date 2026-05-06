Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

Find every expression of type [float]. The pattern [(__ : float)]
matches by type, not just by syntax. So the [int] expression on line
4 is correctly excluded.

  $ $MERLIN single ocamlgrep -query '(__ : float)' < /dev/null \
  >   | jq -r '.value.findings[]? | "\(.file):\(.line):\(.context)"'
  main.ml:3:let f () = 1.0 +. 2.0
  main.ml:5:let h () = sqrt 9.0
  main.ml:6:let _used = (f (), g (), h ())
  main.ml:6:let _used = (f (), g (), h ())
