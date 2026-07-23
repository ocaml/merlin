  $ cat >wrong_expected_kind.ml <<'EOF'
  > module Constr = struct
  >   type t = A | B | C
  > 
  >   let get _ _ = A
  > 
  >   let put f = ignore (f () : t)
  > end
  > 
  > module Record = struct
  >   type t = { a : int; b : int; c : int }
  > 
  >   let get _ _ = { a = 0; b = 0; c = 0 }
  > 
  >   let put f = ignore (f () : t)
  > end
  > 
  > module Bool = struct
  >   type t = true | false
  > 
  >   let get _ _ = true
  > 
  >   let put f = ignore (f () : t)
  > end
  > 
  > module List = struct
  >   type 'a t = [] | (::) of 'a * 'a t
  > 
  >   let get _ _ = []
  > 
  >   let put f = ignore (f () : int t)
  > end
  > 
  > module Unit = struct
  >   [@@@warning "-redefining-unit"]
  >   type t = ()
  > 
  >   let get _ _ = ()
  > 
  >   let put f = ignore (f (() : unit) : t)
  > end
  > 
  > let () =
  >   match (fun x -> Constr.get () x) with
  >   | A | B | C -> ()
  > 
  > 
  > let () =
  >   match (fun x -> Record.get () x) with
  >   | { a; _ } -> ()
  > 
  > 
  > let () =
  >   match (fun x -> Bool.get () x) with
  >   | true -> ()
  > 
  > 
  > let () =
  >   match (fun x -> Bool.get () x) with
  >   | false -> ()
  > 
  > 
  > let () =
  >   match (fun x -> List.get () x) with
  >   | [] -> ()
  >   | _ :: _ -> ()
  > 
  > 
  > let () =
  >   match (fun x -> Unit.get () x) with
  >   | () -> ()
  > 
  > 
  > let () = Constr.put A
  > let () = Record.put { a = 0; b = 0; c = 0 }
  > let () = Bool.put true
  > let () = Bool.put false
  > let () = List.put []
  > let () = List.put [1; 2; 3]
  > let () = Unit.put ()
  > let () = ignore ((Record.get ()).a)
  > let () = (Record.get ()).a <- 5
  > let () = ignore { (Record.get ()) with a = 5 }
  > let foo x = Record.put { x with a = 5 }
  > EOF

  $ $MERLIN single errors -filename wrong_expected_kind.ml < wrong_expected_kind.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 44,
          "col": 4
        },
        "end": {
          "line": 44,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a constructor, the expected type is 'a -> Constr.t"
      },
      {
        "start": {
          "line": 44,
          "col": 8
        },
        "end": {
          "line": 44,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a constructor, the expected type is 'a -> Constr.t"
      },
      {
        "start": {
          "line": 44,
          "col": 12
        },
        "end": {
          "line": 44,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a constructor, the expected type is 'a -> Constr.t"
      },
      {
        "start": {
          "line": 49,
          "col": 4
        },
        "end": {
          "line": 49,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a record, the expected type is 'a -> Record.t"
      },
      {
        "start": {
          "line": 54,
          "col": 4
        },
        "end": {
          "line": 54,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a boolean literal, the expected type is
  'a -> Bool.t"
      },
      {
        "start": {
          "line": 59,
          "col": 4
        },
        "end": {
          "line": 59,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a boolean literal, the expected type is
  'a -> Bool.t"
      },
      {
        "start": {
          "line": 64,
          "col": 4
        },
        "end": {
          "line": 64,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a list literal, the expected type is
  'a -> 'b List.t"
      },
      {
        "start": {
          "line": 65,
          "col": 4
        },
        "end": {
          "line": 65,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a list literal, the expected type is
  'a -> 'b List.t"
      },
      {
        "start": {
          "line": 70,
          "col": 4
        },
        "end": {
          "line": 70,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern should not be a unit literal, the expected type is 'a -> Unit.t"
      },
      {
        "start": {
          "line": 73,
          "col": 20
        },
        "end": {
          "line": 73,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a constructor, the expected type is
  unit -> Constr.t"
      },
      {
        "start": {
          "line": 74,
          "col": 20
        },
        "end": {
          "line": 74,
          "col": 43
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a record, the expected type is unit -> Record.t"
      },
      {
        "start": {
          "line": 75,
          "col": 18
        },
        "end": {
          "line": 75,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a boolean literal, the expected type is
  unit -> Bool.t"
      },
      {
        "start": {
          "line": 76,
          "col": 18
        },
        "end": {
          "line": 76,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a boolean literal, the expected type is
  unit -> Bool.t"
      },
      {
        "start": {
          "line": 77,
          "col": 18
        },
        "end": {
          "line": 77,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a list literal, the expected type is
  unit -> int List.t"
      },
      {
        "start": {
          "line": 78,
          "col": 18
        },
        "end": {
          "line": 78,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a list literal, the expected type is
  unit -> int List.t"
      },
      {
        "start": {
          "line": 79,
          "col": 18
        },
        "end": {
          "line": 79,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a unit literal, the expected type is
  unit -> Unit.t"
      },
      {
        "start": {
          "line": 80,
          "col": 17
        },
        "end": {
          "line": 80,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a -> Record.t which is not a record type."
      },
      {
        "start": {
          "line": 81,
          "col": 9
        },
        "end": {
          "line": 81,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a -> Record.t which is not a record type."
      },
      {
        "start": {
          "line": 82,
          "col": 18
        },
        "end": {
          "line": 82,
          "col": 33
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a -> Record.t which is not a record type."
      },
      {
        "start": {
          "line": 83,
          "col": 23
        },
        "end": {
          "line": 83,
          "col": 39
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a record, the expected type is unit -> Record.t"
      }
    ],
    "notifications": []
  }
