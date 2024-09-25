It can provide signature help after a function-type value.

  $ $MERLIN single signature-help -position 2:11 <<EOF
  > let map = ListLabels.map
  > let _ = map
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
          "parameters": [
            {
              "label": [
                6,
                18
              ]
            },
            {
              "label": [
                22,
                29
              ]
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

It can provide signature help for an operator.

  $ $MERLIN single signature-help -position 2:13 <<EOF
  > let (+) = (+)
  > let _ = 1 + 2
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "(+) : int -> int -> int",
          "parameters": [
            {
              "label": [
                6,
                9
              ]
            },
            {
              "label": [
                13,
                16
              ]
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

It can provide signature help for an anonymous function.

  $ $MERLIN single signature-help -position 1:26 <<EOF
  > let _ = (fun x -> x + 1)
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "_ : int -> int",
          "parameters": [
            {
              "label": [
                4,
                7
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

It can make the non-labelled parameter active.

  $ $MERLIN single signature-help -position 2:14 <<EOF
  > let map = ListLabels.map
  > let _ = map []
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
          "parameters": [
            {
              "label": [
                6,
                18
              ]
            },
            {
              "label": [
                22,
                29
              ]
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

It can make the labelled parameter active.
  $ $MERLIN single signature-help -position 2:14 <<EOF
  > let map = ListLabels.map
  > let _ = map ~f:Int.abs
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "map : f:(int -> int) -> int list -> int list",
          "parameters": [
            {
              "label": [
                6,
                20
              ]
            },
            {
              "label": [
                24,
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

It can make a labelled parameter active by prefix.

  $ $MERLIN single signature-help -position 2:15 <<EOF
  > let mem = ListLabels.mem
  > let _ = mem ~se
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "mem : 'a -> set:'a list -> bool",
          "parameters": [
            {
              "label": [
                6,
                8
              ]
            },
            {
              "label": [
                12,
                23
              ]
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

It can make an optional parameter active by prefix.

  $ $MERLIN single signature-help -position 2:18 <<EOF
  > let create = Hashtbl.create
  > let _ = create ?ra
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "create : ?random:bool -> int -> ('a, 'b) Hashtbl.t",
          "parameters": [
            {
              "label": [
                9,
                21
              ]
            },
            {
              "label": [
                25,
                28
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

It shouldn't give a signature-help when outside of signature.

  $ $MERLIN single signature-help -position 1:8 <<EOF
  > let my_fun x = 1
  > EOF
  {
    "class": "return",
    "value": {},
    "notifications": []
  }
