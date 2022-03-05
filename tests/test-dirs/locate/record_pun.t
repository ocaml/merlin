############
With punning
############

  $ cat > func.ml <<EOF
  > module M = struct
  >   type t = { foo : int }
  > 
  >   let foo = 42
  > end
  > 
  > let foo = 43
  > 
  > let m = { M.foo }
  > 
  > EOF

With punning we jump to the definition of the value and not the record:
  $ $MERLIN single locate -look-for ml -position 9:14 \
  > -filename ./func.ml < ./func.ml |
  > jq '.value.pos'
  {
    "line": 4,
    "col": 6
  }

###############
Without punning
###############

  $ cat > func.ml <<EOF
  > module M = struct
  >   type t = { foo : int }
  > 
  >   let foo = 42
  > end
  > 
  > let foo = 43
  > 
  > let m = { M.foo }
  > 
  > EOF

FIXME should jump to the re-definition of `foo` line 7
  $ $MERLIN single locate -look-for ml -position 9:14  \
  > -filename ./func.ml < ./func.ml |
  > jq '.value.pos'
  {
    "line": 4,
    "col": 6
  }
