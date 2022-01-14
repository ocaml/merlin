  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >lib.mli <<EOF
  > (****************)
  > (* SOME LICENCE *)
  > (****************)
  > 
  > (** Documentation of Lib *)
  > 
  > (** Documentation of Lib.a *)
  > type a = int
  > EOF

  $ cat >lib.ml <<EOF
  > type a = int
  > EOF

  $ cat >libimpl.ml <<EOF
  > (****************)
  > (* SOME LICENCE *)
  > (****************)
  > 
  > (** Documentation of Libimpl *)
  > 
  > (** Documentation of Libimpl.a *)
  > type a = int
  > EOF

  $ cat >main.ml <<EOF
  > type t = Lib.a
  > type u = Libimpl.a
  > EOF

  $ cat >dune <<EOF
  > (executable (name main))
  > EOF

  $ dune build ./main.exe

The licence is correctly ignored when looking for the doc of Lib
  $ $MERLIN single document -position 1:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of Lib",
    "notifications": []
  }

Same when the doc is in the ml file
  $ $MERLIN single document -position 2:11 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of Libimpl",
    "notifications": []
  }
