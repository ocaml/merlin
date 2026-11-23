Reproduction of issue 2123:
https://github.com/ocaml/merlin/issues/2123

  $ cat >record.ml <<'EOF'
  > type t = { legacy : int [@deprecated]; current : int }
  > let get (r : t) = r.legacy
  > let make : t = { legacy = 1; current = 2 }
  > EOF

FIXME: completing a deprecated field in a field access should report
"deprecated": true.
  $ $MERLIN single complete-prefix -position 2:23 -prefix r.leg \
  > -filename record.ml <record.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"legacy","kind":"Label","deprecated":false}

FIXME: same thing in a record construction.
  $ $MERLIN single complete-prefix -position 3:20 -prefix leg \
  > -filename record.ml <record.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"legacy","kind":"Label","deprecated":false}

A non-deprecated field must stay non-deprecated.
  $ $MERLIN single complete-prefix -position 3:31 -prefix cur \
  > -filename record.ml <record.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"current","kind":"Label","deprecated":false}

The [ocaml.deprecated] spelling, with a qualified record.

  $ cat >qualified.ml <<'EOF'
  > module M = struct
  >   type t = { old_field : int [@ocaml.deprecated "use new_field"]; new_field : int }
  > end
  > let f (r : M.t) = r.M.old_field
  > let g ({ M.old_field; _ } : M.t) = old_field
  > EOF

FIXME: field access through a module path.
  $ $MERLIN single complete-prefix -position 4:27 -prefix r.M.ol \
  > -filename qualified.ml <qualified.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"old_field","kind":"Label","deprecated":false}

In a record pattern, the field is completed through another code path
(environment lookup) and is already correctly reported as deprecated.
  $ $MERLIN single complete-prefix -position 5:14 -prefix M.ol \
  > -filename qualified.ml <qualified.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"old_field","kind":"Label","deprecated":true}

For comparison, deprecated constructors and values are correctly reported.

  $ cat >others.ml <<'EOF'
  > type u = Legacy_cstr [@deprecated] | Current_cstr
  > module N : sig val legacy_value : int [@@deprecated] end = struct
  >   let legacy_value = 1
  > end
  > let _ = Leg
  > let _ = N.legacy_v
  > EOF

  $ $MERLIN single complete-prefix -position 5:11 -prefix Leg \
  > -filename others.ml <others.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"Legacy_cstr","kind":"Constructor","deprecated":true}

  $ $MERLIN single complete-prefix -position 6:18 -prefix N.legacy_v \
  > -filename others.ml <others.ml | jq -c '.value.entries[] | {name, kind, deprecated}'
  {"name":"legacy_value","kind":"Value","deprecated":true}
