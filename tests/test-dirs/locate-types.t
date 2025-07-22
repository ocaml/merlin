Test the locate-types command

Create a function that runs locate-types on a variable of a given type
  $ run () {
  >   type="$1"
  > 
  >   # Create a file that creates a variable of the given type. We also define some
  >   # types for us to be able to reference.
  >   cat > foo.ml <<EOF
  > type a
  > type b
  > type c
  > type 'a one_arg
  > type ('a, 'b) two_arg
  > type aliased = t
  > let () =
  >   (* This double Obj.magic is to avoid a kind error *)
  >   let foo : $type = (Obj.magic (Obj.magic 0)) 0 in
  >   ()
  > EOF
  > 
  >   $MERLIN single locate-types -position "9:7" -filename foo.ml < foo.ml \
  >     | jq .value[1] \
  >     | jq -r '
  >         def format_node:
  >           if .data[0] == "Type_ref" then
  >             # Check if result is null or position info is missing
  >             if .data[1].result == null or .data[1].result[2] == null then
  >               # No position info available
  >               .data[1].type_
  >             else
  >               # Extract type_, line number, and calculate column
  >               .data[1].type_ + " (" + 
  >               (.data[1].result[2].pos_lnum | tostring) + ":" + 
  >               ((.data[1].result[2].pos_cnum - .data[1].result[2].pos_bol) | tostring) + 
  >               ")"
  >             end
  >           else
  >             .data[0]
  >           end;
  >         
  >         def process_tree($indent):
  >           format_node as $node_text |
  >           $indent + $node_text + 
  >           if .children | length > 0 then
  >             "\n" + (.children | map(process_tree($indent + "  ")) | join("\n"))
  >           else
  >             ""
  >           end;
  >         
  >         process_tree("")
  >         '
  > }

  $ run2 () {
  >   type="$1"
  > 
  >   # Create a file that creates a variable of the given type. We also define some
  >   # types for us to be able to reference.
  >   cat > foo.ml <<EOF
  > type a
  > type b
  > type c
  > type 'a one_arg
  > type ('a, 'b) two_arg
  > type aliased = t
  > let () =
  >   (* This double Obj.magic is to avoid a kind error *)
  >   let foo : $type = (Obj.magic (Obj.magic 0)) 0 in
  >   ()
  > EOF
  > 
  >   $MERLIN single locate-types -position "9:7" -filename foo.ml < foo.ml \
  >     | jq .value
  > }

Basic type constructors

  $ run "a"
  a (1:5)

  $ run "a one_arg"
  one_arg (4:8)
    a (1:5)

  $ run "(a, b) two_arg"
  two_arg (5:14)
    a (1:5)
    b (2:5)
  $ run "(b, a) two_arg"
  two_arg (5:14)
    b (2:5)
    a (1:5)

Functions

  $ run "a -> b -> c"
  Arrow
    a (1:5)
    b (2:5)
    c (3:5)

  $ run "x:a -> ?y:b -> c"
  Arrow
    a (1:5)
    option
      b (2:5)
    c (3:5)

Tuples

  $ run "a * b * c"
  Tuple
    a (1:5)
    b (2:5)
    c (3:5)

  $ run "a * b"
  Tuple
    a (1:5)
    b (2:5)

Type variables

  $ run "_ one_arg"
  one_arg (4:8)

  $ run "'a one_arg"
  one_arg (4:8)

Objects

  $ run "<x : a; y : b>"
  Object
    a (1:5)
    b (2:5)

Primitive types

  $ run "string"
  string

  $ run "int"
  int

  $ run "a option"
  option
    a (1:5)

  $ run "a list"
  list
    a (1:5)

Compound types

  $ run "(a * b) one_arg"
  one_arg (4:8)
    Tuple
      a (1:5)
      b (2:5)

  $ run "(a option, _) two_arg"
  two_arg (5:14)
    option
      a (1:5)

  $ run "a -> b -> (a * b)"
  Arrow
    a (1:5)
    b (2:5)
    Tuple
      a (1:5)
      b (2:5)

  $ run "((a one_arg) list) option"
  option
    list
      one_arg (4:8)
        a (1:5)
