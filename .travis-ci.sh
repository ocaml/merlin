export OPAMYES=1 OPAMVERBOSE=1
eval `opam config env`

echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

./configure
make
