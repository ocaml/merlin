  $ cat >ill.ml <<EOF
  > module ERROR_locate_from_inside_function_literal_used_as_non_function = struct
  >   (* We expect to end up here:
  >    *  vvvvvvv *)
  >   let problem = \`Problem
  > 
  >   (* We jump to the definition or interface from here...:
  >    *                 vvvvvvv *)
  >   let () = fun () -> problem
  > 
  >   (* ...or from here...:
  >    *                                vvvvvvv *)
  >   let _ : int = Int.succ (fun () -> problem)
  >   
  >   (* ...or from here:
  >    *                                      vvvvvvv *)
  >   let not_a_function : string = fun () -> problem
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
      "line": 8,
      "col": 11
    },
    "end": {
      "line": 8,
      "col": 28
    },
    "type": "typer",
    "sub": [],
    "valid": true,
    "message": "This expression should not be a function, the expected type is unit"
  }

Merlin is still able to inspect part of the ill-typed tree
  $ $MERLIN single type-enclosing -position 8:25 \
  > -filename ill.ml <ill.ml | jq '.value[0]'
  {
    "start": {
      "line": 8,
      "col": 21
    },
    "end": {
      "line": 8,
      "col": 28
    },
    "type": "[> `Problem ]",
    "tail": "no"
  }

And locate works as well
  $ $MERLIN single locate -position 8:25 \
  > -filename ill.ml <ill.ml | jq '.value.pos'
  {
    "line": 4,
    "col": 6
  }
