should be accepted
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {%ext|babar|} *)
  > EOF
  null

should fail
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {%ext id|babar|} *)
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

should accept
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {%ext id|babar|id} *)
  > EOF
  null

should accept
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {id|babar|id} *)
  > EOF
  null

should accept
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {|babar|} *)
  > EOF
  null

should fail
  $ $MERLIN single errors -filename main.ml <<'EOF' | \
  > jq '.value[0]'
  > (* {id|babar|} *)
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
