The "document" command should not return the documentation of a function when
asked about one of its parameters (issue #1809).

  $ document_pos () {
  >   pos="$1"
  >   ocamlmerlin single document -position "$pos" -filename test.ml < test.ml | jq .value -r
  > }

  $ cat > test.ml <<EOF
  > (** some doc *)
  > let foo ?(pp=(fun _ _ -> ())) () =
  >   Format.pp_print_list pp Format.std_formatter []
  > EOF

Get doc for the usage of the optional parameter pp
  $ document_pos 3:24
  No documentation available

Get doc for the definition of pp
  $ document_pos 2:10
  No documentation available

The same happens for any local name whose definition is next to the doc comment:
on the line after it, on the same line, or on the line before it. The documented
function itself must keep its doc.

Labelled parameter
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo ~lbl = lbl + 1
  > EOF
  $ document_pos 2:4
  doc
  $ document_pos 2:15
  No documentation available

Local let binding
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo () = let x = 1 in x
  > EOF
  $ document_pos 2:26
  No documentation available

Plain parameter and pattern variable
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo o = match o with Some v -> v | None -> 0
  > EOF
  $ document_pos 2:8
  No documentation available
  $ document_pos 2:35
  No documentation available

Anonymous function parameter
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo = fun y -> y
  > EOF
  $ document_pos 2:19
  No documentation available

Doc comment after the definition, on the same line
  $ cat > test.ml <<EOF
  > let foo p = p (** doc *)
  > EOF
  $ document_pos 1:4
  doc
  $ document_pos 1:12
  No documentation available

Doc comment on the line after the definition
  $ cat > test.ml <<EOF
  > let foo p = p
  > (** doc *)
  > EOF
  $ document_pos 1:4
  doc
  $ document_pos 1:12
  No documentation available

When the parameter is not on the line right after the doc comment, no doc is
returned
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo
  >   p = 32 + p
  > EOF
  $ document_pos 2:4
  doc
  $ document_pos 3:11
  No documentation available

Names bound by a toplevel pattern are not local and keep the doc
  $ cat > test.ml <<EOF
  > (** doc *)
  > let (a, b) = (1, 2)
  > EOF
  $ document_pos 2:5
  doc

Names bound by a pattern inside a local let are local
  $ cat > test.ml <<EOF
  > (** doc *)
  > let foo () = let (u, w) = (1, 2) in u + w
  > EOF
  $ document_pos 2:18
  No documentation available

Self of an object
  $ cat > test.ml <<EOF
  > (** doc *)
  > let obj = object (self) method a = 1 method b = self#a end
  > EOF
  $ document_pos 2:4
  doc
  $ document_pos 2:48
  No documentation available

Class parameter
  $ cat > test.ml <<EOF
  > (** doc *)
  > class c (x : int) = object method m = x end
  > EOF
  $ document_pos 2:38
  No documentation available

Functor parameter
  $ cat > test.ml <<EOF
  > module type S = sig type t val v : int end
  > 
  > (** doc *)
  > module F (X : S) (Y : S) = struct let w = X.v + Y.v end
  > EOF
  $ document_pos 4:7
  doc
  $ document_pos 4:10
  No documentation available
  $ document_pos 4:48
  No documentation available

Functor parameter in a module type
  $ cat > test.ml <<EOF
  > module type S = sig type t val v : int end
  > 
  > (** doc *)
  > module type T = functor (X : S) -> sig val w : X.t end
  > EOF
  $ document_pos 4:12
  doc
  $ document_pos 4:47
  No documentation available

Functor parameter in an interface
  $ cat > test.mli <<EOF
  > module type S = sig type t val v : int end
  > 
  > (** doc *)
  > module F (X : S) : sig val w : X.t end
  > EOF
  $ ocamlmerlin single document -position 4:7 -filename test.mli < test.mli \
  >   | jq .value -r
  doc
  $ ocamlmerlin single document -position 4:31 -filename test.mli < test.mli \
  >   | jq .value -r
  No documentation available

Locally abstract type
  $ cat > test.ml <<EOF
  > (** doc *)
  > let g (type a) (x : a) : a = x
  > EOF
  $ document_pos 2:4
  doc
  $ document_pos 2:20
  No documentation available

Local module
  $ cat > test.ml <<EOF
  > let h () =
  >   (** doc *)
  >   let module M = struct let z = 1 end in
  >   M.z
  > EOF
  $ document_pos 4:2
  No documentation available

Local exception
  $ cat > test.ml <<EOF
  > let h () =
  >   (** doc *)
  >   let exception E of int in
  >   try raise (E 1) with E k -> k
  > EOF
  $ document_pos 4:13
  No documentation available

FIXME: x is not local but the doc comment belongs to M, not to x
  $ cat > test.ml <<EOF
  > (** doc *)
  > module M = struct let x = 1 end
  > let _ = M.x
  > EOF
  $ document_pos 2:7
  doc
  $ document_pos 2:22
  doc
  $ document_pos 3:10
  doc

Completion with documentation goes through the same code
  $ cat > test.ml <<EOF
  > (** some doc *)
  > let foo ?(pp=(fun _ _ -> ())) () = pp
  > EOF
  $ ocamlmerlin single complete-prefix -prefix pp -position 2:37 -doc y \
  >   -filename test.ml < test.ml | jq -c '.value.entries[] | {name, info}'
  {"name":"pp","info":""}
