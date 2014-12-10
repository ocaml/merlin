OPAM_DEPENDS="yojson ocamlfind"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.00,1.2) ppa=avsm/ocaml40+opam12 ;;
4.01,1.2) ppa=avsm/ocaml41+opam12 ;;
4.02,1.2) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

# opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1
opam init
opam install ${OPAM_DEPENDS}

eval `opam config env`
./configure
make
