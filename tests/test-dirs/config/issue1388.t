  $ cat >.merlin <<EOF
  > STDLIB /my/std
  > FLG -I +../cerberus/flags
  > S +../cerberus/source
  > B +../cerberus/build
  > EOF

  $ echo "" | $MERLIN single dump-configuration -filename test.ml | \
  > grep -A2 'include_dirs\|source_path\|build_path'
        "include_dirs": [
          "+../cerberus/flags"
        ],
  --
        "build_path": [
          "/my/cerberus/build"
        ],
        "source_path": [
          "/my/cerberus/source"
        ],

  $ echo "" | $MERLIN single dump -what paths -filename test.ml 
  {
    "class": "return",
    "value": [
      "$TESTCASE_ROOT",
      "/my/cerberus/flags",
      "/my/cerberus/build",
      "/my/std"
    ],
    "notifications": []
  }
