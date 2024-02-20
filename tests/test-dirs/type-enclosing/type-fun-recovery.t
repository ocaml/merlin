1. Here the expected type `int` is not even a function
  $ cat >test.ml <<'EOF'
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF

1.1
  $ $MERLIN single type-enclosing -position 4:10 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 4,
      "col": 9
    },
    "end": {
      "line": 4,
      "col": 12
    },
    "type": "int",
    "tail": "no"
  }

1.2 Should we expect Merlin to infer that x is of type `int` ?
  $ $MERLIN single type-enclosing -position 1:7 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 6
    },
    "end": {
      "line": 1,
      "col": 9
    },
    "type": "'a",
    "tail": "no"
  }

2. Here the argument does not have the expected type:
  $ cat >test.ml <<'EOF'
  > let f : int -> int = fun x -> match x with
  >   | Some 5 -> 4
  >   | None -> 3
  >   | Some _aa -> 4
  > EOF

  $ $MERLIN single type-enclosing -position 4:10 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 4,
      "col": 4
    },
    "end": {
      "line": 4,
      "col": 12
    },
    "type": "int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 1:36 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 36
    },
    "end": {
      "line": 1,
      "col": 37
    },
    "type": "int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 3:6 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 3,
      "col": 4
    },
    "end": {
      "line": 3,
      "col": 8
    },
    "type": "'a option",
    "tail": "no"
  }

3. Here the return type is not the expected one
  $ cat >test.ml <<'EOF'
  > let f x : int = 
  >  let () = ignore (2 * x) in
  >  3.14
  > EOF

  $ $MERLIN single type-enclosing -position 2:22 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 2,
      "col": 22
    },
    "end": {
      "line": 2,
      "col": 23
    },
    "type": "int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 1:4 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 4
    },
    "end": {
      "line": 1,
      "col": 5
    },
    "type": "int -> int",
    "tail": "no"
  }

4. Here we bind twice the same type
  $ cat >test.ml <<'EOF'
  > let f (type t t) x : int = 
  >  let () = ignore (2 * x) in
  >  3.14
  > EOF

  $ $MERLIN single type-enclosing -position 1:4 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 4
    },
    "end": {
      "line": 1,
      "col": 5
    },
    "type": "int -> int",
    "tail": "no"
  }

  $ cat >test.ml <<'EOF'
  > let f (x : t) : int = 
  >  let () = ignore (2 * x) in
  >  3.14
  > EOF

  $ $MERLIN single type-enclosing -position 1:4 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 4
    },
    "end": {
      "line": 1,
      "col": 5
    },
    "type": "int -> int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 2:22 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 2,
      "col": 22
    },
    "end": {
      "line": 2,
      "col": 23
    },
    "type": "int",
    "tail": "no"
  }

  $ cat >test.ml <<'EOF'
  > let f ?(x : int) : int = 
  >  let () = ignore (2 * x) in
  >  3.14
  > EOF

  $ $MERLIN single type-enclosing -position 1:4 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 1,
      "col": 4
    },
    "end": {
      "line": 1,
      "col": 5
    },
    "type": "?x:'a -> int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 2:22 \
  > -filename under.ml <test.ml | jq '.value[0]' 
  {
    "start": {
      "line": 2,
      "col": 22
    },
    "end": {
      "line": 2,
      "col": 23
    },
    "type": "int",
    "tail": "no"
  }
