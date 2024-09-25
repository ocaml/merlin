This test comes from: https://github.com/janestreet/merlin-jst/pull/59

  $ cat > .merlin <<EOF
  > B build/dir
  > S source/dir
  > BH build-hidden/dir
  > SH source-hidden/dir
  > STDLIB /stdlib
  > SOURCE_ROOT /root
  > EOF

  $ FILE=$(pwd)/test.ml; dot-merlin-reader <<EOF | sed 's#[0-9]*:#?:#g'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:B?:$TESTCASE_ROOT/build/dir)(?:S?:$TESTCASE_ROOT/source/dir)(?:ERROR?:Unknown tag in .merlin?: BH)(?:ERROR?:Unknown tag in .merlin?: SH)(?:STDLIB?:/stdlib)(?:SOURCE_ROOT?:/root))

  $ echo | $MERLIN single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin'
  {
    "build_path": [
      "$TESTCASE_ROOT/build/dir"
    ],
    "source_path": [
      "$TESTCASE_ROOT/source/dir"
    ],
    "cmi_path": [],
    "cmt_path": [],
    "flags_applied": [],
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
    "stdlib": "/stdlib",
    "source_root": "/root",
    "unit_name": null,
    "wrapping_prefix": null,
    "reader": [],
    "protocol": "json",
    "log_file": null,
    "log_sections": [],
    "flags_to_apply": [],
    "failures": [
      "Unknown tag in .merlin: SH",
      "Unknown tag in .merlin: BH"
    ],
    "assoc_suffixes": [
      {
        "extension": ".re",
        "reader": "reason"
      },
      {
        "extension": ".rei",
        "reader": "reason"
      }
    ],
    "cache_lifespan": "5"
  }

  $ rm .merlin
