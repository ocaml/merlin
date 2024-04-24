  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {%ext|babar|} *)
  > EOF
  {
    "start": {
      "line": 1,
      "col": 0
    },
    "end": {
      "line": 1,
      "col": 2
    },
    "type": "typer",
    "sub": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 2
        },
        "message": "String literal begins here"
      }
    ],
    "valid": true,
    "message": "This comment contains an unterminated string literal"
  }
