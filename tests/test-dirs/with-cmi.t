Since OCaml 5.1 the compiler support the -cmi-file flag:
> -cmi-file filename
>     Use the given interface file to type-check the ML source file to compile.
>     When this option is not specified, the compiler looks for a .mli file with
>     the same base name than the implementation it is compiling and in the same
>     directory. If such a file is found, the compiler looks for a corresponding
>     .cmi file in the included directories and reports an error if it fails to
>     find one. 

  $ cat >main.mli <<'EOF'
  > val f : unit -> int
  > EOF

  $ $OCAMLC -c main.mli
  $ rm main.mli


  $ cat >main.ml <<'EOF'
  > let f () = 42
  > EOF

  $ $OCAMLC -c -cmi-file main.cmi main.ml

Merlin should ignore the -cmi-file flag
  $ $MERLIN single errors -cmi-file main.cmi -filename main.ml <main.ml |
  > jq '.value'
  []
