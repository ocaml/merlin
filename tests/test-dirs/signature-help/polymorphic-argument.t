The cursor is right after the function name: the first (polymorphic) parameter
is active and displayed with its quantifier.
  $ $MERLIN single signature-help -position 2:9 << EOF
  > let f (g : 'a. 'a -> 'a) x = (g x, g 0)
  > let _ = f
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : ('a. 'a -> 'a) -> 'b -> 'b * int",
          "parameters": [
            {
              "label": [
                4,
                18
              ]
            },
            {
              "label": [
                22,
                24
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
