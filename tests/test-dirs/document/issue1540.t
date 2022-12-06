  $ cat >doc.mli <<EOF
  > (** whatever decl *)
  > val id : unit -> unit
  > EOF

  $ cat >doc.ml <<EOF
  > (** whatever *)
  > let id () = ()
  > let _ = id ()
  > EOF

FIXME: Merlin should return the docstring
  $ $MERLIN single document -position 2:5 \
  > -filename doc.ml <doc.ml | jq '.value'
  "No documentation available"

  $ $MERLIN single document -position 3:9 \
  > -filename doc.ml <doc.ml 
  {
    "class": "return",
    "value": "whatever decl",
    "notifications": []
  }
