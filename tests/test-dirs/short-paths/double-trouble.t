Found in issue #1913

  $ cat >test.ml <<EOF
  > type _ plus = | Zero : 'm plus |  Suc : 'm plus -> 'm plus
  > type _ has_plus = Plus : 'm plus -> unit has_plus;;
  > let (Plus (type mn3) (ed : mn3 plus) ) = Plus (Suc Zero) in ed + 2
  > EOF

  $ $MERLIN single errors -short-paths -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 60
        },
        "end": {
          "line": 3,
          "col": 62
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value ed has type mn3 plus but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
