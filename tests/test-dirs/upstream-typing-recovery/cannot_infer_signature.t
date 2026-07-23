  $ cat >cannot_infer_signature.ml <<'EOF'
  > module type S = sig
  >   val x : int
  > end
  > 
  > module U = struct
  >   let f =
  >     let m = (module struct let x = 10 end) in
  >     let n = (module struct let x = 10 end : S) in
  >     m, n
  > end
  > 
  > let k () =
  >   let module A = (val fst (U.f)) in
  >   let module D = (val fst (U.f) : S) in
  >   D.x + D.x + 12
  > 
  > let r : string = k ()
  > EOF

  $ $MERLIN single errors -filename cannot_infer_signature.ml < cannot_infer_signature.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 12
        },
        "end": {
          "line": 7,
          "col": 42
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The signature for this packaged module couldn't be inferred."
      },
      {
        "start": {
          "line": 13,
          "col": 17
        },
        "end": {
          "line": 13,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The signature for this packaged module couldn't be inferred."
      },
      {
        "start": {
          "line": 17,
          "col": 17
        },
        "end": {
          "line": 17,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
