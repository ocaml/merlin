  $ cat > .merlin <<EOF
  > EXCLUDE_QUERY_DIR
  > FLG -pp 'I/definitly/need/quoting.exe -nothing'
  > FLG -ppx '/path/to/ppx.exe --as-ppx --cookie '\\''library-name="model"'\\'''
  > FLG -w @3
  > EOF

  $ FILE=$(pwd)/test.ml; dot-merlin-reader <<EOF | sed 's#[0-9]*:#?:#g'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:EXCLUDE_QUERY_DIR)(?:FLG(?:-pp?:I/definitly/need/quoting.exe -nothing))(?:FLG(?:-ppx?:/path/to/ppx.exe --as-ppx --cookie 'library-name="model"'))(?:FLG(?:-w?:@3)))

  $ echo | $MERLIN single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin'
  {
    "build_path": [],
    "source_path": [],
    "cmi_path": [],
    "cmt_path": [],
    "flags_applied": [
      {
        "workdir": "$TESTCASE_ROOT",
        "workval": [
          "-pp",
          "I/definitly/need/quoting.exe -nothing"
        ]
      },
      {
        "workdir": "$TESTCASE_ROOT",
        "workval": [
          "-ppx",
          "/path/to/ppx.exe --as-ppx --cookie 'library-name=\"model\"'"
        ]
      },
      {
        "workdir": "$TESTCASE_ROOT",
        "workval": [
          "-w",
          "@3"
        ]
      }
    ],
    "extensions": [],
    "suffixes": [
      {
        "impl": ".ml",
        "intf": ".mli"
      },
      {
        "impl": ".re",
        "intf": ".rei"
      }
    ],
    "stdlib": null,
    "reader": [],
    "protocol": "json",
    "log_file": null,
    "log_sections": [],
    "flags_to_apply": [],
    "failures": [],
    "assoc_suffixes": [
      {
        "extension": ".re",
        "reader": "reason"
      },
      {
        "extension": ".rei",
        "reader": "reason"
      }
    ]
  }

  $ rm .merlin
