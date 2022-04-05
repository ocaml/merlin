  $ cat > predef.ml <<EOF
  > true
  > EOF

  $ $MERLIN single locate -look-for ml -position 1:2 \
  > -filename ./predef.ml < ./predef.ml
  {
    "class": "return",
    "value": "\"true\" is a builtin, and it is therefore impossible to jump to its definition",
    "notifications": []
  }
