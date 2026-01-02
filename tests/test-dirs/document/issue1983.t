The "document" command sometimes concatenates documentation for consecutive entries of variants or records.

  $ document_pos () {
  >   pos="$1"
  >   ocamlmerlin single document -position "$pos" < test.ml | jq .value -r
  > }

  $ cat > test.ml <<EOF
  > type variant =
  >   | A (** A *)
  >   | B (** B *)
  >   | C (** C *)
  > 
  > type record =
  >   { e : int (** e *)
  >   ; f : int (** f *)
  >   ; g : int (** g *)
  >   }
  > 
  > let _ = A, B, C
  > let _f x = x.e, x.f, x.g
  > EOF

Get doc for A
FIXME: this should just be "A"
  $ document_pos 12:9
  A
  B

Get doc for B
FIXME: this should just be "B"
  $ document_pos 12:12
  B
  C

Get doc for C
  $ document_pos 12:15
  C

Get doc for e
FIXME: this should just be "e"
  $ document_pos 13:14
  e
  f

Get doc for f
FIXME: this should just be "f"
  $ document_pos 13:19
  e
  f
  g

Get doc for g
FIXME: this should just be "g"
  $ document_pos 13:24
  f
  g

  $ cat > test.ml <<EOF
  > type variant =
  >   | A
  >   (** A *)
  >   | B
  >   (** B *)
  >   | C
  >   (** C *)
  > 
  > type record =
  >   { e : int
  >     (** e *)
  >   ; f : int
  >     (** f *)
  >   ; g : int
  >     (** g *)
  >   }
  > 
  > let _ = A, B, C
  > let _f x = x.e, x.f, x.g
  > EOF

Get doc for A
  $ document_pos 18:9
  A

Get doc for B
  $ document_pos 18:12
  B

Get doc for C
  $ document_pos 18:15
  C

Get doc for e
  $ document_pos 19:14
  e

Get doc for f
FIXME: this should just be "f"
  $ document_pos 19:19
  e
  f

Get doc for g
FIXME: this should just be "g"
  $ document_pos 19:24
  f
  g
