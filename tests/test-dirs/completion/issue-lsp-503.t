
  $ cat >test.ml <<'EOF'
  > [1;2]|>List.ma
  > EOF

We complete the name of the object

  $ $MERLIN single complete-prefix -position 1:14 -prefix "List.ma" \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "map",
          "kind": "Value",
          "desc": "('a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "mapi",
          "kind": "Value",
          "desc": "(int -> 'a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "map2",
          "kind": "Value",
          "desc": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }
