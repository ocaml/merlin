(enabled_if (< %{ocaml_version} 4.08.0))
FIXME
  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some 3 -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "match match (None : int option) with | Some 3 -> () | exception _ -> () with
  | () -> (??)"
    ],
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
