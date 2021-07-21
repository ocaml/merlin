  $ cat >bar.ml <<EOF
  > module M = struct
  >  include _
  >  let x = 3
  > end
  > let _ = M.x
  > let _ = M.foo
  > EOF

  $ $MERLIN single locate -look-for ml -position 5:11 -filename bar.ml <bar.ml |
  > jq '.value.pos'
  {
    "line": 3,
    "col": 5
  }

  $ $MERLIN single locate -look-for ml -position 6:11 -filename bar.ml <bar.ml |
  > jq '.value'
  "Not in environment 'M.foo'"

  $ $MERLIN single locate -look-for ml -position 2:10 -filename bar.ml <bar.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }
