  $ cat >ill.ml <<EOF
  > module ERROR_locate_from_non_function_being_applied_sometimes = struct
  >   let remove_duplicates : 'a list -> eq:('a -> 'a -> bool) -> 'a list =
  >     fun _ ~eq:_ ->
  >       (* Pretend we have an implementation *)
  >       []
  >   ;;
  > 
  >   (* We expect to end up here:
  >    *  vvvvvvv *)
  >   let problem = \`Problem
  >   let cmp a b = Float.compare a b
  > 
  >   (* We jump from here:
  >    *                                                              vvvvvvv *)
  >   let () = remove_duplicates ~eq:Int.equal (ListLabels.sort ~cmp (problem 1.0))
  > 
  >   (* We get the error message "Not in environment 'problem'" and go nowhere. *)
  > end
  > EOF

When some typing error happens

  $ $MERLIN single errors \
  > -filename ill.ml <ill.ml |
  > tr '\r\n' ' ' | jq '.value[0]'
  {
    "start": {
      "line": 15,
      "col": 33
    },
    "end": {
      "line": 15,
      "col": 42
    },
    "type": "typer",
    "sub": [],
    "valid": true,
    "message": "This expression has type int -> int -> bool but an expression was expected of type Float.t -> Float.t -> bool Type int is not compatible with type Float.t = float"
  }

Merlin is still able to inspect part of the ill-typed tree
  $ $MERLIN single type-enclosing -position 15:70 \
  > -filename ill.ml <ill.ml | tr '\r\n' ' ' | jq '.value[0]'
  {
    "start": {
      "line": 15,
      "col": 66
    },
    "end": {
      "line": 15,
      "col": 73
    },
    "type": "[> `Problem ]",
    "tail": "no"
  }

FIXME: And locate should as well...
  $ $MERLIN single locate -position 15:70 \
  > -filename ill.ml <ill.ml | jq '.value'
  "Not in environment 'problem'"
