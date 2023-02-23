  $ cat >ill.ml <<EOF
  > module ERROR_locate_from_incorrectly_refutable_match = struct
  >   (* We expect to end up here.
  >    *  vvvvvvv *)
  >   let problem = \`Problem
  > 
  >   let () =
  >     (* We jump to defn or intf from here:
  >      *    vvvvvvv *)
  >     match problem with
  >     | _ -> .
  >   ;;
  > 
  >   (* We get the message "Not in environment 'problem'" and go nowhere. *)
  > end
  > EOF

When a typing error happens:
  $ $MERLIN single errors \
  > -filename ill.ml <ill.ml |
  > tr '\r\n' ' ' | jq '.value[0]'
  {
    "start": {
      "line": 10,
      "col": 6
    },
    "end": {
      "line": 10,
      "col": 7
    },
    "type": "typer",
    "sub": [],
    "valid": true,
    "message": "This match case could not be refuted. Here is an example of a value that would reach it: _"
  }

Merlin is still able to give the type of an identifier involved in the error:
  $ $MERLIN single type-enclosing -position 9:13 \
  > -filename ill.ml <ill.ml | jq '.value[0]'
  {
    "start": {
      "line": 9,
      "col": 10
    },
    "end": {
      "line": 9,
      "col": 17
    },
    "type": "[> `Problem ]",
    "tail": "no"
  }

And locate finds its definition:
  $ $MERLIN single locate -position 9:13 \
  > -filename ill.ml <ill.ml | jq '.value.pos'
  {
    "line": 4,
    "col": 6
  }
