OPAM_DEPENDS="yojson ocamlfind ppx_deriving"
export OPAMYES=1 OPAMVERBOSE=1

echo System OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam switch $OCAML_VERSION
opam install ${OPAM_DEPENDS}
