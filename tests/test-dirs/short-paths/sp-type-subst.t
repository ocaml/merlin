  $ cat >uchar_intf.ml <<'EOF'
  > module type Uchar = sig 
  >   type t (* t 275 *) = Uchar.t (* t 276 *)
  >   type ucharmeleon := int 
  >   
  >   module Decode_result : sig
  >     type t = Uchar.utf_decode (* t 303 *)
  > 
  >     val is_valid : t -> bool (* t 303 *) 
  >   end
  > end
  > EOF 

Surprisingly, in Printtyp, the ident for the inner t has a different stamp...
(306). It's not clear how we could have the correct ident in the discourse.

When using short-paths, more than ever, `t` should be printed as `t` 
and not `Uchar.utf_decode`. Removing the `type uch.. := int` fixes the issue.
  $ $MERLIN single type-enclosing -position 10:1 -short-paths \
  > -filename uchar_intf.ml < uchar_intf.ml | tr '\n' ' ' | jq '.value[0].type'
  "sig   type t = Uchar.t   module Decode_result :     sig type t = Uchar.utf_decode val is_valid : t -> bool end end"


  $ cat >uchar_intf.ml <<'EOF'
  > module type Uchar = sig 
  >   type t (* t 275 *) = Uchar.t (* t 276 *)
  >   type ucharmeleon := int 
  >   
  >   module Decode_result : sig
  >     type tlong = Uchar.utf_decode (* t 303 *)
  >     type t = tlong
  > 
  >     val is_valid : tlong -> bool (* t 303 *) 
  >   end
  > end
  > EOF 

FIXME Here we also expect is_valid: t -> bool
  $ $MERLIN single type-enclosing -position 11:1 -short-paths \
  > -filename uchar_intf.ml < uchar_intf.ml | tr '\n' ' ' | jq '.value[0].type'
  "sig   type t = Uchar.t   module Decode_result :     sig       type tlong = Uchar.utf_decode       type t = tlong       val is_valid : tlong -> bool     end end"


Example from the manual
  $ cat >tsubst.ml <<'EOF'
  > module type S = sig
  >   type t
  >   module Sub : sig
  >     type outer := t
  >     type t
  >     val to_outer : t -> outer
  >   end
  > end
  > EOF

  $ $MERLIN single type-enclosing -position 6:10 -short-paths \
  > -filename tsubst.ml < tsubst.ml | tr '\n' ' ' | jq '.value[0].type'
  "t -> outer"



  $ $MERLIN single type-enclosing -position 8:1 -short-paths \
  > -filename tsubst.ml < tsubst.ml | tr '\n' ' ' | jq '.value[0].type'
  "sig type t module Sub : sig type t val to_outer : t -> t/2 end end"
