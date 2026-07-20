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
  > let _interp = "hello %{name} and hello // and %{ // }"
  > let _interp = {|hello %{name} and hello // and %{ // } |}
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

Interpolation state is correctly closed in double-quoted strings:

  $ $MERLIN single document -position 8:40 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

Interpolation state is correctly re-opened in double-quoted strings:

  $ $MERLIN single document -position 8:50 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of my operator.",
    "notifications": []
  }

Identifiers inside [%{...}] interpolations in {||} strings (as used by
string interpolation ppxes) are still reconstructed and looked up:

  $ $MERLIN single document -position 9:24 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "The name of the world.",
    "notifications": []
  }

Interpolation state is correctly closed in {||} strings:

  $ $MERLIN single document -position 9:41 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

Interpolation state is correctly re-opened in {||} strings:

  $ $MERLIN single document -position 9:51 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Documentation of my operator.",
    "notifications": []
  }


Escaped quotes do not terminate a string, and an escaped backslash right
before the closing quote does not prevent it from terminating the string:

  $ cat >escapes.ml <<'EOF'
  > (** Documentation of my operator. *)
  > let ( // ) a b = a + b
  > let one = 1
  > (** The name of the world. *)
  > let name = "world"
  > let _escaped = "say \"hi//there\" more %{name}"
  > let _backslash = "ends with an escaped backslash \\"
  > let _use = one // 2
  > EOF

The [//] of [\"hi//there\"] sits between two escaped quotes and is still
string content:

  $ $MERLIN single document -position 6:24 -filename escapes.ml <escapes.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

So is the word [more] after the escaped quotes:

  $ $MERLIN single document -position 6:34 -filename escapes.ml <escapes.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

The [%{name}] interpolation after the escaped quotes still works:

  $ $MERLIN single document -position 6:41 -filename escapes.ml <escapes.ml
  {
    "class": "return",
    "value": "The name of the world.",
    "notifications": []
  }

The [//] on the last line is real code: this only works if the string ending
in an escaped backslash was terminated by its closing quote:

  $ $MERLIN single document -position 8:15 -filename escapes.ml <escapes.ml
  {
    "class": "return",
    "value": "Documentation of my operator.",
    "notifications": []
  }
