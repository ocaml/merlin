Testing array desugaring

  $ $MERLIN single errors -filename array_good.ml <<EOF
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

FIXME UPGRADE 5.2: this test show additionnal warnings after the 5.2 upgrade; probably a
type that should be marked as incorrect is not anymore.
  $ $MERLIN single errors -filename array_bad.ml <<EOF
  > module Array = struct end
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value Array.get"
      },
      {
        "start": {
          "line": 2,
          "col": 15
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename array_fake_good.ml <<EOF
  > module Array = struct let get _ _ = () end
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single errors -filename unsafe_array_good.ml -unsafe <<EOF
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

FIXME UPGRADE 5.2: this test show additionnal warnings after the 5.2 upgrade; probably a
type that should be marked as incorrect is not anymore.
  $ $MERLIN single errors -filename unsafe_array_bad.ml -unsafe <<EOF
  > module Array = struct end
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value Array.unsafe_get"
      },
      {
        "start": {
          "line": 2,
          "col": 15
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename unsafe_array_fake_good.ml -unsafe <<EOF
  > module Array = struct let unsafe_get _ _ = () end
  > let x = [|0|].(0)
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
