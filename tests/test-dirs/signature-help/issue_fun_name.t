  $ cat > test.ml <<'EOF'
  > let v = List.map Fun.id []
  > EOF

Valid
  $ $MERLIN single signature-help -position 1:4 -filename test < test.ml
  {
    "class": "return",
    "value": {},
    "notifications": []
  }

  $ $MERLIN single signature-help -position 1:18 -filename test < test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
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

  $ $MERLIN single signature-help -position 1:21 -filename test < test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
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

  $ $MERLIN single signature-help -position 1:24 -filename test < test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
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
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }

  $ cat > t.ml <<'EOF'
  > module M : sig
  >   val f : int -> unit
  > end = struct
  >   let f (_ : int) = ()
  > end
  > 
  > let () = M.f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 7:13 -filename test < t.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "M.f : int -> unit",
          "parameters": [
            {
              "label": [
                6,
                9
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


FIXME: Signature help should not appear on the name of the function:
  $ $MERLIN single signature-help -position 1:9 -filename test < test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
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

  $ $MERLIN single signature-help -position 1:14 -filename test < test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
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
