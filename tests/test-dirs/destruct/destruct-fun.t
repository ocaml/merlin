Test case-analysis in the middle of a [fun].

  $ cat >fun.ml <<EOF
  > let f x (bb : bool) y = something
  > EOF

  $ $MERLIN single case-analysis -start 1:10 -end 1:11 \
  > -log-file - -filename fun.ml <fun.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 9
        },
        "end": {
          "line": 1,
          "col": 11
        }
      },
      "((false as bb) : bool)|((true as bb) : bool)"
    ],
    "notifications": []
  }
