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

  $ $MERLIN single signature-help -position 1:18 -filename test < test.ml \
  > | jq '.value | {label: .signatures[0].label, activeParameter: .activeParameter}'
  {
    "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
    "activeParameter": 0
  }

  $ $MERLIN single signature-help -position 1:21 -filename test < test.ml \
  > | jq '.value | {label: .signatures[0].label, activeParameter: .activeParameter}'
  {
    "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
    "activeParameter": 0
  }

  $ $MERLIN single signature-help -position 1:24 -filename test < test.ml \
  > | jq '.value | {label: .signatures[0].label, activeParameter: .activeParameter}'
  {
    "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
    "activeParameter": 1
  }

  $ $MERLIN single signature-help -position 1:9 -filename test < test.ml
  {
    "class": "return",
    "value": {},
    "notifications": []
  }

  $ $MERLIN single signature-help -position 1:14 -filename test < test.ml
  {
    "class": "return",
    "value": {},
    "notifications": []
  }

  $ cat > test2.ml <<'EOF'
  > module M : sig
  >   val f : int -> unit
  > end = struct
  >   let f (_ : int) = ()
  > end
  > 
  > let () = M.f (* keep whitespace *)
  > EOF

  $ $MERLIN single signature-help -position 7:13 -filename test < test2.ml \
  > | jq '.value | {label: .signatures[0].label, activeParameter: .activeParameter}'
  {
    "label": "M.f : int -> unit",
    "activeParameter": 0
  }
