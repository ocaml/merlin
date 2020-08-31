(enabled_if (and (>= %{ocaml_version} 4.10.0) (< %{ocaml_version} 4.11.0)))
FIXME
  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF | grep -B 1 Invalid_argument \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some 3 -> () \
  > EOF
    "class": "exception",
    "value": "Invalid_argument(\"Parmatch.Pattern_head.deconstruct: (exception P)\")

FIXME
  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF | grep -B 1 Invalid_argument \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some _ -> () \
  > EOF
    "class": "exception",
    "value": "Invalid_argument(\"Parmatch.Pattern_head.deconstruct: (exception P)\")
