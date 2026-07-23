  $ cat >unexpected_existential.ml <<'EOF'
  > type any = Any: 'a -> any
  > 
  > let Any x = Any ()
  > 
  > let () =
  >   let Any x = Any () and () = () in
  >   ()
  > 
  > let () =
  >   let rec Any x = Any () in
  >   ()
  > 
  > let () =
  >   let[@attribute] Any x = Any () in
  >   ()
  > 
  > class c (Any x) = object end
  > 
  > 
  > class d = object(Any x) end
  > 
  > class e = let Any _x = () in object end
  > 
  > module T = struct
  >   let Any x = Any ()
  > 
  >   let () =
  >     let Any x = Any () and () = () in
  >     ()
  > 
  >   let () =
  >     let rec Any x = Any () in
  >     ()
  > 
  >   let () =
  >     let[@attribute] Any x = Any () in
  >     ()
  > 
  >   class c (Any x) = object end
  > 
  > 
  >   class d = object(Any x) end
  > 
  >   class e = let Any _x = () in object end
  > 
  > end
  > EOF

  $ $MERLIN single errors -filename unexpected_existential.ml < unexpected_existential.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in toplevel bindings,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 6,
          "col": 6
        },
        "end": {
          "line": 6,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in grouped (let ... and ...) bindings,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 10,
          "col": 10
        },
        "end": {
          "line": 10,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only variables are allowed as left-hand side of let rec"
      },
      {
        "start": {
          "line": 14,
          "col": 18
        },
        "end": {
          "line": 14,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in presence of attributes,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 17,
          "col": 8
        },
        "end": {
          "line": 17,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in class arguments,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 20,
          "col": 16
        },
        "end": {
          "line": 20,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in self patterns,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 22,
          "col": 14
        },
        "end": {
          "line": 22,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in bindings inside class definition,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 25,
          "col": 6
        },
        "end": {
          "line": 25,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in toplevel bindings,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 28,
          "col": 8
        },
        "end": {
          "line": 28,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in grouped (let ... and ...) bindings,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 32,
          "col": 12
        },
        "end": {
          "line": 32,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only variables are allowed as left-hand side of let rec"
      },
      {
        "start": {
          "line": 36,
          "col": 20
        },
        "end": {
          "line": 36,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in presence of attributes,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 39,
          "col": 10
        },
        "end": {
          "line": 39,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in class arguments,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 42,
          "col": 18
        },
        "end": {
          "line": 42,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in self patterns,
  but the constructor Any introduces existential types."
      },
      {
        "start": {
          "line": 44,
          "col": 16
        },
        "end": {
          "line": 44,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types are not allowed in bindings inside class definition,
  but the constructor Any introduces existential types."
      }
    ],
    "notifications": []
  }
