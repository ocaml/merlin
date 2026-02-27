The cursor is between 'map' and 'in' and behaves correctly as the in is written.
  $ cat > test1.ml <<'EOF'
  > let _ =
  > let f = List.map   in
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test1 < test1.ml | jq '.value.signatures[0].label'
  "List.map : ('a -> 'b) -> 'a list -> 'b list"

The cursor is after the map and triggers the 'map\ signature help.
  $ cat > test2.ml <<'EOF'
  > let _ =
  > let f = List.map  
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test2 < test2.ml | jq '.value.signatures[0].label'
  "List.map : ('a -> 'b) -> 'a list -> 'b list"

The cursor is after 'map' and triggers correctly signature help for the function 'map'.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map 
  > let n = 5 in
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test2 < test2.ml
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

The cursor is after the 'ends_with' function and triggers the signature help.
FIXME: The active parameter should be 0.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map ~f:(fun x -> String.ends_with ) in
  > let n = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:47 -filename test2 < test2.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "String.ends_with : suffix:string -> string -> bool",
          "parameters": [
            {
              "label": [
                19,
                32
              ]
            },
            {
              "label": [
                36,
                42
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

The cursor is between 'map' and 'in' and triggers correctly the 'map' even if the unfinished 'iter'
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map  in
  > let s = List.iter
  > let n = 5 in
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test2 < test2.ml
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

The cursor is after 'map' and triggers correctly the 'map'.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map 
  > let s = List.iter
  > let n = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test2 < test2.ml
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

FIXME: The cursor is after the '()' of the 'iter' function. It triggers the function from the parenthesis but should trigger the 'iter' with active parameter set to 1.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map 
  > let s = List.iter () 
  > let n = 5 in
  > EOF

  $ $MERLIN single signature-help -position 3:21 -filename test2 < test2.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "_ : 'a list -> unit",
          "parameters": [
            {
              "label": [
                4,
                11
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

The cursor is after the 'iter' and trigger correcly the 'iter'.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map 
  > let s = List.iter 
  > let n = 5 in
  > EOF

  $ $MERLIN single signature-help -position 3:18 -filename test2 < test2.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.iter : ('a -> unit) -> 'a list -> unit",
          "parameters": [
            {
              "label": [
                12,
                24
              ]
            },
            {
              "label": [
                28,
                35
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

The cursor is between the '()' and 'in' and triggers correctly the 'iter' signature help.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.map 
  > let s = List.iter ()  in
  > let n = 5 in
  > EOF

  $ $MERLIN single signature-help -position 3:21 -filename test2 < test2.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "List.iter : ('a -> unit) -> 'a list -> unit",
          "parameters": [
            {
              "label": [
                12,
                24
              ]
            },
            {
              "label": [
                28,
                35
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

The cursor is after the 'in' of the 'let n = 5 in' and triggers correctly nothing.
  $ cat > test2.ml <<'EOF'
  > let g =
  > let f = List.hd in
  > let s = List.iter  in
  > let n = 5 in
  > 9
  > EOF

  $ $MERLIN single signature-help -position 4:18 -filename test2 < test2.ml
  {
    "class": "return",
    "value": {},
    "notifications": []
  }
