  $ $MERLIN single errors -filename main.ml -unboxed-types
  {
    "class": "return",
    "value": [
      {
        "type": "config",
        "sub": [],
        "valid": true,
        "message": "unknown flag -unboxed-types"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename main.ml -no-unboxed-types
  {
    "class": "return",
    "value": [
      {
        "type": "config",
        "sub": [],
        "valid": true,
        "message": "unknown flag -no-unboxed-types"
      }
    ],
    "notifications": []
  }
