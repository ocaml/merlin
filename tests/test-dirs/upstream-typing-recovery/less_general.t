  $ cat >less_general.ml <<'EOF'
  > let poly (id: 'a. 'a -> 'a) =
  >   let a = id 3
  >   and b = id 3.0
  >   and c = id "three"
  >   in (a, b, c)
  > 
  > let _ok1 = poly (fun x -> x)
  > let _less_general_1 = poly (fun x -> x + 1)
  > 
  > let _ = poly (let r = ref None in fun x -> r := Some x; x)
  > 
  > class ['a] id1 = object
  >   method virtual id : 'b. 'b -> 'a
  >   method id x = x
  > end
  > EOF

  $ $MERLIN single errors -filename less_general.ml < less_general.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 27
        },
        "end": {
          "line": 8,
          "col": 43
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This argument has type int -> int which is less general than 'a. 'a -> 'a
  The type int is not a type variable."
      },
      {
        "start": {
          "line": 10,
          "col": 13
        },
        "end": {
          "line": 10,
          "col": 58
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This argument has type 'a -> 'a which is less general than 'a0. 'a0 -> 'a0
  The type variable 'a0 is not generalizable to an universal type variable."
      },
      {
        "start": {
          "line": 14,
          "col": 12
        },
        "end": {
          "line": 14,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This method has type 'b -> 'b which is less general than 'b0. 'b0 -> 'b
  The type variable 'b0 is not generalizable to an universal type variable."
      }
    ],
    "notifications": []
  }
