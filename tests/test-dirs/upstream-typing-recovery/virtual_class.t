  $ cat >virtual_class.ml <<'EOF'
  > class virtual t = object
  >   method virtual foo : string -> string
  > end
  > 
  > class virtual k = object
  >   method virtual foo : string -> string
  >   method virtual bar : unit
  > end
  > 
  > class r = object
  >   inherit k
  >   method foo x = x
  >   method bar = ()
  > end
  > 
  > 
  > let f = new t
  > let g = new k
  > 
  > let r =
  >   let a = new t in
  >   let b = new r in
  >   (b # foo) (a # foo "Hello World")
  > 
  > let x = new r
  > 
  > let t: bool = 10
  > EOF

  $ $MERLIN single errors -filename virtual_class.ml < virtual_class.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 17,
          "col": 8
        },
        "end": {
          "line": 17,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot instantiate the virtual class t"
      },
      {
        "start": {
          "line": 18,
          "col": 8
        },
        "end": {
          "line": 18,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot instantiate the virtual class k"
      },
      {
        "start": {
          "line": 21,
          "col": 10
        },
        "end": {
          "line": 21,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot instantiate the virtual class t"
      },
      {
        "start": {
          "line": 23,
          "col": 21
        },
        "end": {
          "line": 23,
          "col": 34
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      },
      {
        "start": {
          "line": 27,
          "col": 14
        },
        "end": {
          "line": 27,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type bool"
      }
    ],
    "notifications": []
  }
