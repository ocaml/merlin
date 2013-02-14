#!/bin/bash

# ocamlmerlin must be in path
out=`mktemp`

ocamlmerlin < ./tests/$1.in  > $out
diff $out ./tests/$1.out
