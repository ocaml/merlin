  $ cat >main.ml <<EOF
  > module B = struct
  >   module type T = sig end
  > 
  >   module T = struct end
  > end
  > 
  > module M : B.T = struct end
  > module type T2 = B.T
  > module M2 : T2 = struct end
  > EOF

  $ $MERLIN single locate -look-for mli -position 7:13 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 2,
    "col": 14
  }

  $ $MERLIN single locate -look-for ml -position 7:13 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 2,
    "col": 14
  }


  $ $MERLIN single locate -look-for mli -position 9:12 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 2,
    "col": 14
  }
