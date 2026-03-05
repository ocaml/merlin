The cursor is after the second `~`. As `~x` has already been written, the active parameter is `y`.
  $ $MERLIN single signature-help -position 2:17 << EOF
  > let f a ?y ~x t = (a, y, x, t)
  > let _ = f ~x:43 ~ 1
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> ?y:'_weak1 -> x:int -> '_weak2 -> int * '_weak1 option * int * '_weak2",
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
                21
              ]
            },
            {
              "label": [
                25,
                30
              ]
            },
            {
              "label": [
                34,
                41
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

The cursor is after the `?`. There is only one optional parameter so the active parameter is y.
  $ $MERLIN single signature-help -position 2:17 << EOF
  > let f a ?y ~x t = (a, y, x, t)
  > let _ = f ~x:43 ? 1
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> ?y:'_weak1 -> x:int -> '_weak2 -> int * '_weak1 option * int * '_weak2",
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
                21
              ]
            },
            {
              "label": [
                25,
                30
              ]
            },
            {
              "label": [
                34,
                41
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

This test is valid because `~y` is a complete parameter.
  $ $MERLIN single signature-help -position 2:18 << EOF
  > let f a ?y ~x t = (a, y, x, t)
  > let _ = f ~x:43 ~y 1
  > EOF
  {
    "class": "return",
    "value": {},
    "notifications": []
  }

The cursor is at the end of the second line. The function is waiting for a last labelled argument and it is `y`.
  $ $MERLIN single signature-help -position 2:20 << EOF
  > let f a ~x ~y t = (a, y, x, t)
  > let _ = f 1 ~x:43 1 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> x:int -> y:'_weak1 -> int -> int * '_weak1 * int * int",
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
                16
              ]
            },
            {
              "label": [
                20,
                29
              ]
            },
            {
              "label": [
                33,
                36
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

The cursor is after the second `~`. As `~x` has already been written, the active parameter is `y`.
  $ $MERLIN single signature-help -position 2:17 << EOF
  > let f a ~x ~y t = (a, y, x, t)
  > let _ = f ~x:43 ~ 1
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> x:int -> y:'_weak1 -> '_weak2 -> int * '_weak1 * int * '_weak2",
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
                16
              ]
            },
            {
              "label": [
                20,
                29
              ]
            },
            {
              "label": [
                33,
                40
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

The cursor is after `~`. As `f` is already given in the expression, signature help waits for the next parameter as `~f` is a valid parameter.
  $ $MERLIN single signature-help -position 2:13 <<EOF
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
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

The cursor is after `~`. As `f` isn't already given in the expression, signature help waits for the value of `~f`.
  $ $MERLIN single signature-help -position 2:13 <<EOF
  > let map = ListLabels.map
  > let _ = map ~
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
      "activeParameter": 0,
      "activeSignature": 0
    },
    "notifications": []
  }
