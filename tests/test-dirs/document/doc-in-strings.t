Identifiers should not be reconstructed from the contents of string literals:
merlin used to answer queries about the [/] of ["/usr/local/home"] with the
type and documentation of the integer division operator.

  $ cat >main.ml <<'EOF'
  > (** Documentation of my operator. *)
  > let ( // ) a b = a + b
  > let _ = 1 // 2
  > let _path = "/usr/local//home"
  > let _quoted = {|slash // inside quoted string|}
  > (** The name of the world. *)
  > let name = "world"
  > let _interp = "hello %{name}"
  > EOF

A real operator, outside of a string literal, is documented:

  $ $MERLIN single document -position 3:10 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of my operator.",
    "notifications": []
  }

The same operator inside a double-quoted string literal is not an identifier
(merlin used to reply with the operator's documentation here):

  $ $MERLIN single document -position 4:23 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

Neither are words inside string literals (here, the [local] of
["/usr/local//home"]):

  $ $MERLIN single document -position 4:18 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

The contents of quoted strings are also skipped:

  $ $MERLIN single document -position 5:22 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

But identifiers inside [%{...}] interpolations (as used by string
interpolation ppxes) are still reconstructed and looked up:

  $ $MERLIN single document -position 8:23 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "The name of the world.",
    "notifications": []
  }
