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

FIXME: Signature help does not appear for M.f:

  $ cat >test.ml <<'EOF'
  > 
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

  $ $MERLIN single signature-help -position 7:13 -filename test <test.ml 
  {
    "class": "return",
    "value": {},
    "notifications": []
  }
