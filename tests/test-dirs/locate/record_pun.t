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
    "line": 7,
    "col": 4
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

We jump to the re-definition of `foo` line 7
  $ $MERLIN single locate -look-for ml -position 9:14  \
  > -filename ./func.ml < ./func.ml |
  > jq '.value.pos'
  {
    "line": 7,
    "col": 4
  }

Another identifier with the same last component is not taken for the pun:
  $ cat >other.ml <<'EOF'
  > module X = struct type t = { bar : int } end
  > module Y = struct let bar = 1 end
  > let bar = 2
  > let y = { X.bar }
  > EOF

  $ $MERLIN single locate -look-for ml -prefix Y.bar -position 4:12 \
  > -filename ./other.ml < ./other.ml |
  > jq '.value.pos'
  {
    "line": 2,
    "col": 22
  }
