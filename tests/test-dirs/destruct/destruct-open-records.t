We'll always have a record type with three fields.

When the pattern-match is not exhaustive, we always prefer to make it
exhaustive:

- On the [Some _] wildcard

  $ $MERLIN single case-analysis -start 3:12 -end 3:12 -filename test.ml <<EOF
  > type t = { a : int; b : bool option; c : int }
  > let () = match x with
  >  {b = Some _ ; _ } -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 23
        },
        "end": {
          "line": 3,
          "col": 23
        }
      },
      "
  | { b = None;_} -> _"
    ],
    "notifications": []
  }

- On the [; _] wildcard

  $ $MERLIN single case-analysis -start 3:16 -end 3:16 -filename test.ml <<EOF
  > type t = { a : int; b : bool option; c : int }
  > let () = match x with
  >  {b = Some _ ; _ } -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 23
        },
        "end": {
          "line": 3,
          "col": 23
        }
      },
      "
  | { b = None;_} -> _"
    ],
    "notifications": []
  }

Only when the pattern-match is exhaustive, we look whether we can refine the
pattern at point:

- On the [b = _] wildcard

  $ $MERLIN single case-analysis -start 3:7 -end 3:7 -filename test.ml <<EOF
  > type t = { a : int; b : bool option; c : int }
  > let () = match x with
  >  {b = _ ; _ } -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 1
        },
        "end": {
          "line": 3,
          "col": 13
        }
      },
      "{ b = None;_} | { b = Some _;_}"
    ],
    "notifications": []
  }

- On the [; _] wildcard, it destructs the open record pattern into a closed one.

  $ $MERLIN single case-analysis -start 3:11 -end 3:11 -filename test.ml <<EOF
  > type t = { a : int; b : bool option; c : int }
  > let () = match x with
  >  {b = _ ; _ } -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 1
        },
        "end": {
          "line": 3,
          "col": 13
        }
      },
      "{ b = _; a; c }"
    ],
    "notifications": []
  }

And last but not least, the original example for #436:

  $ $MERLIN single case-analysis -start 2:9 -end 2:9 -filename test.ml <<EOF
  > type r = {a:int; b:int; c:int; d:int;}
  > let f {a;_} = a + b + c + d
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 11
        }
      },
      "{ a; b; c; d }"
    ],
    "notifications": []
  }
