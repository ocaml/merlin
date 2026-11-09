Create the types Box.t and Either.t, each of which are in their own module and file.
Box.t depends on Either.t.

  $ cat >either.ml <<EOF
  > type t = Left | Right
  > EOF

  $ cat >box.ml <<EOF
  > type t = Box of Either.t
  > EOF

  $ $OCAMLC either.ml box.ml

Create the file that we'll use to demonstrate the error

  $ touch foo.ml

In Persistent_env.find_pers_struct, the module Either does not fails anymore to be found:

  $ cat > foo.ml <<EOF
  > let f (x : Box.t) =
  >   match x with
  >   | Box _ -> 10
  > EOF
  $ LOC=3:8
  $ show_location foo.ml $LOC
  let f (x : Box.t) =
    match x with
    | Box █ -> 10
  $ $MERLIN single case-analysis -start 3:8 -end 3:8 -filename foo.ml < foo.ml
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
        }
      },
      "Box (Either.Left) | Box (Either.Right)"
    ],
    "notifications": []
  }

If we explicitly use Either (by opening it), we force it to load and the query works
(also) as expected:

  $ $MERLIN single case-analysis -start 4:8 -end 4:8 -filename foo.ml <<EOF
  > open Either
  > let f (x : Box.t) =
  >   match x with
  >   | Box _ -> 10
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 4
        },
        "end": {
          "line": 4,
          "col": 9
        }
      },
      "Box (Left) | Box (Right)"
    ],
    "notifications": []
  }
