(enabled_if (and (>= %{ocaml_version} 4.08.0) (< %{ocaml_version} 4.10.0)))

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some 3 -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

FIXME
  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some _ -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }
