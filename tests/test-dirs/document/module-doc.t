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

  $ cat >main.ml <<EOF
  > type t = Lib.a
  > EOF

  $ cat >dune <<EOF
  > (executable (name main))
  > EOF

  $ dune build ./main.exe

FIXME: the licence should be ignored ?
  $ $MERLIN single document -position 1:11 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "No documentation available",
    "notifications": []
  }

Without the licence it works as expected:
  $ sed -i -e '1,4d' lib.mli
  $ dune build ./main.exe

  $ $MERLIN single document -position 1:11 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of Lib",
    "notifications": []
  }
