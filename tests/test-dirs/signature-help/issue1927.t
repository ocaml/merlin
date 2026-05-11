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
