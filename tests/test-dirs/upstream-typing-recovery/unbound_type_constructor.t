  $ cat >unbound_type_constructor.ml <<'EOF'
  > let foo1 : int = 3
  > 
  > let foo2 : int * toto = 3, 4
  > 
  > let foo3 : int * int = 3, 4
  > 
  > type tata = Tata of int
  > 
  > let foo4 : tata = Tata (snd foo2)
  > EOF

  $ $MERLIN single errors -filename unbound_type_constructor.ml < unbound_type_constructor.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 17
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor toto"
      }
    ],
    "notifications": []
  }
