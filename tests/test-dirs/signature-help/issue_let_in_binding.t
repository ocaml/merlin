  $ cat > test1.ml <<'EOF'
  > let _ =
  > let f = List.map   in
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test < test1.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'b) -> 'a list -> 'b list",
          "parameters": [
            {
              "label": [
                11,
                21
              ]
            },
            {
              "label": [
                25,
                32
              ]
            }
          ]
        }
      ],
      "activeParameter": 0,
      "activeSignature": 0
    },
    "notifications": []
  }

FIXME: Signature-help does not trigger on unfinished let ... in bindings.
  $ cat > test2.ml <<'EOF'
  > let _ =
  > let f = List.map
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test < test2.ml
  {
    "class": "return",
    "value": {},
    "notifications": []
  }
