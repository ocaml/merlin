  $ mkdir experimental
  $ mkdir unix

  $ cat >experimental/m_intf.ml <<'EOF'
  > module type S = sig val x : int end (* diff *)
  > EOF

  $ cat >experimental/exp.ml <<'EOF'
  > module M_intf = M_intf
  > EOF

  $ cat >unix/m_intf.ml <<'EOF'
  > module type S = sig val x : int end
  > EOF

  $ cat >unix/unix.ml <<'EOF'
  > module M_intf = M_intf
  > EOF

  $ cat >hack.ml <<'EOF'
  > let f (module R : Exp.M_intf.S) =
  >   let _ = R.x in
  >   ()
  > EOF

  $ cd experimental
  $ $OCAMLC -keep-locs -bin-annot  m_intf.ml exp.ml
  $ cd ..

  $ cd unix
  $ $OCAMLC -keep-locs -bin-annot  m_intf.ml unix.ml
  $ cd ..

  $ $OCAMLC -keep-locs -bin-annot -I experimental/ -I linux/ hack.ml
 
  $ $MERLIN single locate -position 2:12 -look-for implementation \
  > -build-path experimental -build-path unix \
  > -source-path . -source-path unix -source-path experimental \
  > -filename hack.ml <hack.ml | jq '.value'
  "Several source files in your path have the same name, and merlin doesn't know which is the right one: $TESTCASE_ROOT/unix/m_intf.ml, $TESTCASE_ROOT/experimental/m_intf.ml"

