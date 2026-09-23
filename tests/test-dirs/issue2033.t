  $ cat >main.mli <<EOF
  > type typ =
  >   | A
  >   | B
  >   | C
  > 
  > val string_of_type : typ -> string
  > EOF

Work as intended:
  $ LOC=5:0
  $ show_location main.mli $LOC
  type typ =
    | A
    | B
    | C
  █
  val string_of_type : typ -> string
  $ $MERLIN single locate -prefix typ -position $LOC -context unknown -look-for mli -filename main.mli <main.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/main.mli",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }

TODO: trying to find the `typ` declaration with the cursor positioned on a signature item located after the type definition fails:
  $ LOC=6:10
  $ show_location main.mli $LOC
  type typ =
    | A
    | B
    | C
  
  val string█of_type : typ -> string
  $ $MERLIN single locate -prefix typ -position $LOC -context unknown -look-for mli -filename main.mli <main.mli
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }
