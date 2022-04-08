  $ mkdir .cmi-dir
  $ mkdir .cmi-dir/lib

  $ cd src/lib

  $ $OCAMLC -pack -o libpack.cmo -bin-annot config.ml lib.ml

  $ ls
  config.cmi
  config.cmo
  config.cmt
  config.ml
  lib.cmi
  lib.cmo
  lib.cmt
  lib.ml
  libpack.cmi
  libpack.cmo
  libpack.cmt

  $ cp libpack.cmi ../../.cmi-dir/lib

  $ cd ../bin

  $ $OCAMLC -o exe.exe -bin-annot -I ../../.cmi-dir/lib \
  > ../lib/libpack.cmo config.ml exe.ml

  $ ./exe.exe
  Libpack.Lib.config_value: 4242,
  Libpack.Config.value: 4242,
  Config.value: 42

  $ cat >.merlin <<EOF
  > CMI ../../.cmi-dir/lib
  > CMI .
  > CMT .
  > CMT ../lib
  > EOF

FIXME should jump to src/lib/lib.ml
  $ $MERLIN single locate -look-for ml -position 2:20 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  "'Libpack.Lib.config_value' seems to originate from 'Lib' whose ML file could not be found"


FIXME should jump to src/lib/config.ml
  $ $MERLIN single locate -look-for ml -position 3:20 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/src/bin/config.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

should jump to src/bin/config.ml
  $ $MERLIN single locate -look-for ml -position 4:13 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/src/bin/config.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
