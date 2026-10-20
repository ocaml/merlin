Partial rebuilds leave the build directory inconsistent for a while. Here
Dep_base has been rebuilt but Dep_tag (compiled against the old Dep_base) has
not been yet. Importing the old Dep_tag correctly fails with "inconsistent
assumptions". Once Dep_tag is rebuilt and everything on disk is consistent
again, the error should go away.
It doesn't: the failed unit is never added to the state's persistent env, so
`Env.check_state_consistency` never re-checks it, and the error stays in the
typer items reused from the cache and in the lazily computed components of the
alias module Import. Only a fresh typer state (here, a new filename) recovers.

  $ $MERLIN server stop-server
  $ mkdir dep
  $ cat >.merlin <<EOF
  > B dep
  > EOF

Utilities
---------

"Rebuild" a dependency. The .cmi is unlinked first so that it gets a fresh
inode, and its mtime is set to a fixed time plus $3 seconds to defeat coarse
mtime granularity (`touch -t` is POSIX, unlike `touch -d`).
  $ build () {
  >   rm -f dep/$1.cmi
  >   printf "$2\n" > dep/$1.mli
  >   $OCAMLC -I dep $4 -c dep/$1.mli
  >   touch -t "200001010000.$(printf %02d $3)" dep/$1.cmi dep
  > }

  $ errors () {
  >   ocamlmerlin server errors -filename $1 < $1 |
  >     jq -r '.value[] | "\(.start.line):\(.start.col) \(.message | gsub("\\s+"; " "))"' |
  >     sed "s:$PWD:\$DIR:g"
  > }

  $ locate () {
  >   ocamlmerlin server locate -position $2 -filename $1 < $1 |
  >     jq -r '"\(.class): \(.value | if type == "object" then "found" else gsub("\\s+"; " ") end)"' |
  >     sed "s:$PWD:\$DIR:g"
  > }

  $ setup () {
  >   rm -f dep/*
  >   build dep_base 'type t = int' 1
  >   build dep_tag 'val add : Dep_base.t -> unit' 1
  >   build import 'module Schedule_tag = Dep_tag' 1 -no-alias-deps
  >   # The rebuild has reached Dep_base but not Dep_tag.
  >   build dep_base 'type t = int\nval new_in_v2 : unit' 2
  > }

Direct reference
----------------

  $ setup
  $ cat >direct.ml <<EOF
  > let _ : Dep_base.t = 1
  > let _ = Dep_tag.add
  > EOF

Setup creates a partial build, we expect inconsistencies
  $ locate direct.ml 2:18
  error: The files $DIR/dep/dep_base.cmi and $DIR/dep/dep_tag.cmi make inconsistent assumptions over interface Dep_base
  $ errors direct.ml
  0:-1 The files $DIR/dep/dep_base.cmi and $DIR/dep/dep_tag.cmi make inconsistent assumptions over interface Dep_base

The rebuild finishes. Locate should work and the error should be gone.
  $ build dep_tag 'val add : Dep_base.t -> unit' 3
  $ locate direct.ml 2:18
  return: found
  $ errors direct.ml

Even after an edit below the reference.
  $ echo 'let _ = ()' >> direct.ml
  $ locate direct.ml 2:18
  return: found
  $ errors direct.ml

An edit above the reference.
  $ (echo '(* edited *)'; cat direct.ml) > tmp && mv tmp direct.ml
  $ locate direct.ml 3:18
  return: found
  $ errors direct.ml

For reference, a fresh document:
  $ cp direct.ml fresh_direct.ml
  $ locate fresh_direct.ml 3:18
  return: found
  $ errors fresh_direct.ml

Through an alias module
-----------------------
  $ setup
  $ cat >alias.ml <<EOF
  > let _ : Dep_base.t = 1
  > open Import
  > let _ = Schedule_tag.add
  > EOF

Setup creates a partial build, we expect inconsistencies
  $ locate alias.ml 3:23
  error: The files $DIR/dep/dep_base.cmi and $DIR/dep/dep_tag.cmi make inconsistent assumptions over interface Dep_base
  $ errors alias.ml
  0:-1 The files $DIR/dep/dep_base.cmi and $DIR/dep/dep_tag.cmi make inconsistent assumptions over interface Dep_base

The rebuild finishes. Locate should work and the error should be gone.
  $ build dep_tag 'val add : Dep_base.t -> unit' 4
  $ locate alias.ml 3:23
  return: found
  $ errors alias.ml

Even after an edit below the reference.
  $ echo 'let _ = ()' >> alias.ml
  $ locate alias.ml 3:23
  return: found
  $ errors alias.ml

An edit above the reference.
  $ (echo '(* edited *)'; cat alias.ml) > tmp && mv tmp alias.ml
  $ locate alias.ml 4:23
  return: found
  $ errors alias.ml

For reference, a fresh document:
  $ cp alias.ml fresh_alias.ml
  $ locate fresh_alias.ml 4:23
  return: found
  $ errors fresh_alias.ml
  $ $MERLIN server stop-server
