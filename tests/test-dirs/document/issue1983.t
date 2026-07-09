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
  f
  g

Get doc for g
  $ document_pos 13:24
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
  $ document_pos 19:19
  f

Get doc for g
  $ document_pos 19:24
  g

  $ cat > test.ml <<EOF
  > type variant =
  >   | A of int (** A *)
  >   | B of int (** B *)
  >   | C of int (** C *)
  > 
  > let _ = A 0, B 0, C 0
  > EOF

Get doc for A
FIXME: this should just be "A"
  $ document_pos 6:9
  A
  B

Get doc for B
FIXME: this should just be "B"
  $ document_pos 6:14
  B
  C

Get doc for C
  $ document_pos 6:19
  C

  $ cat > test.ml <<EOF
  > type variant =
  >   | A of int option (** A *)
  >   | B of int option (** B *)
  >   | C of int option (** C *)
  > 
  > type record =
  >   { e : int option (** e *)
  >   ; f : int option (** f *)
  >   ; g : int option (** g *)
  >   }
  > 
  > let _ = A None, B None, C None
  > let _f x = x.e, x.f, x.g
  > EOF

Get doc for A
FIXME: this should just be "A"
  $ document_pos 12:9
  A
  B

Get doc for B
FIXME: this should just be "B"
  $ document_pos 12:17
  B
  C

Get doc for C
  $ document_pos 12:25
  C

Get doc for e
FIXME: this should just be "e"
  $ document_pos 13:14
  e
  f

Get doc for f
FIXME: this should just be "f"
  $ document_pos 13:19
  f
  g

Get doc for g
  $ document_pos 13:24
  g

  $ cat > test.ml <<EOF
  > type inline_record =
  >   Foo of
  >     { e : int (** e *)
  >     ; f : int (** f *)
  >     ; g : int (** g *)
  >     }
  > 
  > let _f (Foo x) = x.e, x.f, x.g
  > EOF

Get doc for e
FIXME: this should just be "e"
  $ document_pos 8:20
  e
  f

Get doc for f
FIXME: this should just be "f"
  $ document_pos 8:25
  f
  g

Get doc for g
  $ document_pos 8:30
  g

We also need to be careful about doc comments on subsequent lines.

  $ cat > test.ml <<EOF
  > type variant =
  >   | A (** A *)
  >   | B
  >   | C (** C *)
  > 
  > type record =
  >   { e : int (** e *)
  >   ; f : int
  >   ; g : int (** g *)
  >   }
  > 
  > let _ = A, B, C
  > let _f x = x.e, x.f, x.g
  > EOF

Get doc for A
  $ document_pos 12:9
  A

Get doc for B
FIXME: this should be empty
  $ document_pos 12:12
  C

Get doc for C
  $ document_pos 12:15
  C

Get doc for e
  $ document_pos 13:14
  e

Get doc for f
FIXME: this should be empty
  $ document_pos 13:19
  g

Get doc for g
  $ document_pos 13:24
  g
