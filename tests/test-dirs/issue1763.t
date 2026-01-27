  $ $MERLIN single type-enclosing -position 8:0 <<EOF | jq '.value[].type'
  > module Foo = struct
  >   let hidden = 3
  > end
  > 
  > include struct
  >   open Foo
  >   let visible = 4
  > end
  > EOF
  "sig val visible : int end"

  $ $MERLIN single type-enclosing -position 7:0 <<EOF | jq '.value[].type'
  > include struct
  >   module Foo = struct
  >     let hidden = 3
  >   end
  >   open Foo
  >   let visible = 4
  > end
  > EOF
  "sig module Foo : sig val hidden : int end val visible : int end"

This was failing in the issue but appears to not fail anymore with the actual merlin
  $ $MERLIN single type-enclosing -position 6:0 <<EOF | jq '.value[].type'
  > include struct
  >   open struct
  >     let hidden = 3
  >   end
  >   let visible = 4
  > end
  > EOF
  "sig val visible : int end"
