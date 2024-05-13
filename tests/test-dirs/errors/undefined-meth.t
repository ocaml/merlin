  $ cat >main.ml <<EOF
  > let o = object
  >   method a = 1
  >   method b = 2
  >   method cdefg = 2
  > end;;
  > print_int o#a;;
  > print_int o#cdef
  > EOF

  $ $MERLIN single errors -filename main.ml <main.ml |
  > tr '\r\n' ' ' | jq '.value[0].message'
  "This expression has type < a : int; b : int; cdefg : int > It has no method cdef Hint: Did you mean cdefg?"
