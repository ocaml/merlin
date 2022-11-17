Examples adapted from https://v2.ocaml.org/manual/doccomments.html#ss:label-comments.
We do not currently show these comments:

  $ cat >main.ml <<EOF
  > type t1 =
  >   lbl_a:unit (** lbl_a comment *) ->
  >   lbl_b:unit (** lbl_b comment *) -> unit
  > let _test (f: t1) =
  >   let _ = f ~lbl_a:() ~lbl_b:() in
  >   ()
  > EOF

FIXME: expected "lbl_b comment"

  $ $MERLIN single document -position 5:15 \
  > -filename main.ml <main.ml | jq '.value'
  "Not a valid identifier"

  $ cat >main.ml <<EOF
  > type t = <
  >   meth_a: unit; (** meth_a comment *)
  >   meth_b: unit; (** meth_b comment *)
  >   >
  > let _test (o: t3) =
  >   o#meth_b
  > EOF

FIXME: expected "meth_b comment"

  $ $MERLIN single document -position 6:6 \
  > -filename main.ml <main.ml | jq '.value'
  "Not in environment 'meth_b'"

  $ cat >main.ml <<"EOF"
  > type t = [
  >   | `Poly_a (** Poly_a comment *)
  >   | `Poly_b (** Poly_b comment *)
  > ]
  > let _: t4 =
  > `Poly_b
  > EOF

FIXME: expected "Poly_b comment"

  $ $MERLIN single document -position 6:4 \
  > -filename main.ml <main.ml | jq '.value'
  "Not a valid identifier"

FIXME: expected "fld_b comment"

  $ cat >main.ml <<EOF
  > type t2 = {
  >   fld_a: unit; (** fld_a comment *)
  >   fld_b: unit; (** fld_b comment *)
  >   fld_c: unit;
  > }
  > let _ = {
  >   fld_a = ();
  >   fld_b = ();
  >   fld_c = ()
  > }
  > EOF

  $ $MERLIN single document -position 8:4 \
  > -filename main.ml <main.ml | tr '\r\n' ' ' | jq '.value'
  "fld_a comment fld_b comment"
