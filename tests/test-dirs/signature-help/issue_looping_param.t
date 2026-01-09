  $ $MERLIN single signature-help -position 2:14 <<EOF
  > let f x y z = x + y + z
  > let a = f 4 4 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> int -> int -> int",
          "parameters": [
            {
              "label": [
                4,
                7
              ]
            },
            {
              "label": [
                11,
                14
              ]
            },
            {
              "label": [
                18,
                21
              ]
            }
          ]
        }
      ],
      "activeParameter": 2,
      "activeSignature": 0
    },
    "notifications": []
  }

FIXME: Once the function has all of its parameters, it should not loop again on the parameters
  $ $MERLIN single signature-help -position 2:16 <<EOF
  > let f x y z = x + y + z
  > let a = f 4 4 4 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> int -> int -> int",
          "parameters": [
            {
              "label": [
                4,
                7
              ]
            },
            {
              "label": [
                11,
                14
              ]
            },
            {
              "label": [
                18,
                21
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
