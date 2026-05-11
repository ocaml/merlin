  $ cat >test.ml <<'EOF'
  > module M : sig
  >   val f : int -> unit
  > end = struct
  >   let f (_ : int) = ()
  > end
  > 
  > let () = M.f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 7:13 -filename test <test.ml 
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

Regression for #1927: signature help on a function whose type is reached through a type abbreviation (`type t = int -> unit`).

  $ cat >test.ml <<'EOF'
  > type t = int -> unit
  > 
  > module M : sig
  >   val f : t
  > end = struct
  >   let f (_ : int) = ()
  > end
  > 
  > let () = M.f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 9:13 -filename test <test.ml
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

  $ cat >test.ml <<'EOF'
  > type t = int * int
  > 
  > let f (_ : t) = ()
  > 
  > let () = f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 5:11 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : t -> unit",
          "parameters": [
            {
              "label": [
                4,
                5
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

  $ cat >test.ml <<'EOF'
  > type t = int -> int 
  > 
  > let f (a : int) (op : t) : int = op a
  > 
  > let _ = f 1 (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 5:12 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> t -> int",
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
                12
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


  $ cat >test.ml <<'EOF'
  > type t = int -> unit
  > 
  > let f (_ : int) : t = fun (_ : int) -> ()
  > 
  > let () = f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 5:11 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> t",
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

  $ cat >test.ml <<'EOF'
  > type t = int -> unit
  > 
  > let f (_ : int) : t = fun (_ : int) -> ()
  > 
  > let _ = f 1 2 (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 5:13 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> int -> unit",
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
            }
          ]
        }
      ],
      "activeParameter": 1,
      "activeSignature": 0
    },
    "notifications": []
  }
 

  $ cat >test.ml <<'EOF'
  > type t = int -> int -> unit
  > 
  > let f (_ : int) : t = fun (_ : int) -> ()
  > 
  > let _ = f 1 2 (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 5:14 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : int -> int -> int -> unit",
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

  $ cat >test.ml <<'EOF'
  > type t = x:int -> int
  > let f : t = fun ~x -> x + 1
  > 
  > let _ = f ~(* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 4:10 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : x:int -> int",
          "parameters": [
            {
              "label": [
                4,
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

  $ cat >test.ml <<'EOF'
  > type t = s:string -> count:int -> unit 
  > 
  > let f : t = fun ~s:_ ~count:_ -> ()
  > 
  > let _ = f ~s:"hello" 
  > EOF

  $ $MERLIN single signature-help -position 5:21 -filename test <test.ml
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "f : s:string -> count:int -> unit",
          "parameters": [
            {
              "label": [
                4,
                12
              ]
            },
            {
              "label": [
                16,
                25
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
