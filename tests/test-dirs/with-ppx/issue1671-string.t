  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > let f x =
  >   [%string
  >     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \
  >      %{x}"]
  > ;;
  > print_endline @@ f "42";;
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps ppx_string)))
  > EOF

  $ dune build @check 

  $ dune exec ./main.exe
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa      42

FIX upstream: locs issued by the ppx does not enable Merlin to work as expected
  $ $MERLIN single type-enclosing -position 3:7 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 97
        },
        "type": "string -> string",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Merlin should ignore hidden nodes in occurrences results
  $ $MERLIN single occurrences -identifier-at 1:6 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 7
        }
      }
    ],
    "notifications": []
  }
