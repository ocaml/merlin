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
  > S ../lib
  > EOF

OK should jump to src/lib/lib.ml
  $ $MERLIN single locate -look-for ml -position 2:20 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/src/lib/lib.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

OK should show the doc from src/lib/lib.ml
  $ $MERLIN single document -position 2:20 \
  > -filename ./exe.ml <exe.ml  | jq '.value'
  "doc of lib.config_value"


OK should jump to src/lib/config.ml
  $ $MERLIN single locate -look-for ml -position 3:20 \
  > -log-file log.txt -log-section locate -filename ./exe.ml <exe.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/src/lib/config.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

$ cat log.txt | grep -v '^#'
Libpack.Config.value
inferred context: expression
looking for the source of 'Libpack.Config.value' (prioritizing .ml files)
lookup in value namespace
found: 'Libpack!.Config.value' in namespace value with uid Config.0
initial: CU Libpack . "Config"[module] . "value"[value]
inspecting Libpack
Found file $TESTCASE_ROOT/src/lib/libpack.cmt
shapes for Libpack loaded from $TESTCASE_ROOT/src/lib/libpack.cmt
$TESTCASE_ROOT/src/lib
~/.opam/4.14.0/lib/ocaml
inspecting Config
Found file $TESTCASE_ROOT/src/lib/config.cmt
shapes for Config loaded from $TESTCASE_ROOT/src/lib/config.cmt
reduced: <Config.0>
Loading the shapes for unit "Config"
Found file $TESTCASE_ROOT/src/lib/config.cmt
$TESTCASE_ROOT/src/lib/config.cmt
Shapes successfully loaded, looking for Config.0
Found location: File "config.ml", line 1, characters 4-9
attempt to find "config.ml"
multiple matches in the source path : $TESTCASE_ROOT/src/lib/config.ml , $TESTCASE_ROOT/src/bin/config.ml
... trying to use source digest to find the right one
Source digest: 76df683f14daff1623c558ccabc2ac0a
$TESTCASE_ROOT/src/lib/config.ml (76df683f14daff1623c558ccabc2ac0a)
found: $TESTCASE_ROOT/src/lib/config.ml

FIXME should show the doc from src/lib/config.ml
  $ $MERLIN single document -position 3:20 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  "doc of bin.config.value"

OK should jump to src/bin/config.ml
  $ $MERLIN single locate -look-for ml -position 4:13 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/src/bin/config.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

OK should show the doc of bin.config.value
  $ $MERLIN single document -position 4:13 \
  > -filename ./exe.ml <exe.ml | jq '.value'
  "doc of bin.config.value"
