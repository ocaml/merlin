  $ mkdir lib_a
  $ mkdir lib_b

  $ cd lib_a
  $ cat > repro3_lib_a__.ml-gen << 'EOF'
  > module Header_version = Repro3_lib_a__Header_version
  > EOF

  $ cat >header_version.mli <<'EOF'
  > type t = V1
  > EOF

  $ cat > repro3_lib_a.ml << 'EOF'
  > module Header_version = Header_version
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o repro3_lib_a__.cmo -c -impl repro3_lib_a__.ml-gen
  $ $OCAMLC -o repro3_lib_a__Header_version.cmi -c -intf header_version.mli -open Repro3_lib_a__
  $ $OCAMLC -o repro3_lib_a.cmi -c -impl repro3_lib_a.ml -open Repro3_lib_a__

  $ ls
  header_version.mli
  repro3_lib_a.cmi
  repro3_lib_a.cmo
  repro3_lib_a.ml
  repro3_lib_a__.cmi
  repro3_lib_a__.cmo
  repro3_lib_a__.ml-gen
  repro3_lib_a__Header_version.cmi

  $ cd ../lib_b

  $ cat >repro3_lib_b.ml <<'EOF'
  > include struct
  >   module Header_version = Repro3_lib_a.Header_version
  > end
  > 
  > module Foo = struct
  >   type t = { header : Header_version.t }
  > end
  > EOF

  $ $OCAMLC -o repro3_lib_b.cmi -c -impl repro3_lib_b.ml -I ../lib_a

  $ ls
  repro3_lib_b.cmi
  repro3_lib_b.cmo
  repro3_lib_b.ml

  $ cd ..

  $ cat >import.ml << 'EOF'
  > module Header_version = Repro3_lib_a.Header_version
  > module State = Repro3_lib_b
  > EOF

  $ $OCAMLC -o import.cmi -c -impl import.ml -I lib_a -I lib_b

  $ cat >repro3_usage.ml << 'EOF'
  > open Import
  > let f (t : State.Foo.t) = t.header 
  > EOF

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > B lib_a
  > B lib_b
  > EOF

  $ $MERLIN single type-enclosing -position 2:30 -index 0 \
  > -filename repro3_usage.ml <repro3_usage.ml | jq '.value[0].type'
  "Header_version.t"
