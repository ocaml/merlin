Same tests as in recent.t, but a different output

No double underscore because the stdlib does not have them.
We should probably rely on our own.

  $ echo "" | $MERLIN single search-by-polarity -query "-float +int64" \
  > -position 1:0 -filename test.ml | \
  > jq '.value.entries[] | del(.info) | del(.kind)'
  {
    "name": "Int64.bits_of_float",
    "desc": "float -> int64"
  }
  {
    "name": "Int64.of_float",
    "desc": "float -> int64"
  }

There is less duplication than on versions >= 4.07, but there still is some.

  $ echo "" | $MERLIN single search-by-polarity -safe-string \
  > -query "-int +string" -position 1:0 -filename test.ml | \
  > head -n16
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "string_of_int",
          "kind": "Value",
          "desc": "int -> string",
          "info": ""
        },
        {
          "name": "string_of_int",
          "kind": "Value",
          "desc": "int -> string",
          "info": ""
        },

- Lower bound on function arity

  $ echo "" | $MERLIN single search-by-polarity \
  > -query "-float +fun +fun +float" -position 1:0 -filename test.ml | \
  > jq '.value.entries[] | del(.info) | del(.kind)'
  {
    "name": "**",
    "desc": "float -> float -> float"
  }
  {
    "name": "**",
    "desc": "float -> float -> float"
  }
  {
    "name": "*.",
    "desc": "float -> float -> float"
  }
  {
    "name": "*.",
    "desc": "float -> float -> float"
  }
  {
    "name": "+.",
    "desc": "float -> float -> float"
  }
  {
    "name": "+.",
    "desc": "float -> float -> float"
  }
  {
    "name": "-.",
    "desc": "float -> float -> float"
  }
  {
    "name": "-.",
    "desc": "float -> float -> float"
  }
  {
    "name": "/.",
    "desc": "float -> float -> float"
  }
  {
    "name": "/.",
    "desc": "float -> float -> float"
  }
  {
    "name": "atan2",
    "desc": "float -> float -> float"
  }
  {
    "name": "atan2",
    "desc": "float -> float -> float"
  }
  {
    "name": "copysign",
    "desc": "float -> float -> float"
  }
  {
    "name": "copysign",
    "desc": "float -> float -> float"
  }
  {
    "name": "hypot",
    "desc": "float -> float -> float"
  }
  {
    "name": "hypot",
    "desc": "float -> float -> float"
  }
  {
    "name": "ldexp",
    "desc": "float -> int -> float"
  }
  {
    "name": "ldexp",
    "desc": "float -> int -> float"
  }
  {
    "name": "mod_float",
    "desc": "float -> float -> float"
  }
  {
    "name": "mod_float",
    "desc": "float -> float -> float"
  }
  {
    "name": "Random.State.float",
    "desc": "Random.State.t -> float -> float"
  }
