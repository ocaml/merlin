  $ cat >.merlin <<EOF
  > B foobar_dir
  > FOOBAR bar
  > EOF

  $ $MERLIN single errors -filename test.ml <<EOF
  > let x = 2
  > EOF
  {
    "class": "return",
    "value": [
      {
        "type": "config",
        "sub": [],
        "valid": true,
        "message": "Unknown tag in .merlin: FOOBAR"
      }
    ],
    "notifications": []
  }
