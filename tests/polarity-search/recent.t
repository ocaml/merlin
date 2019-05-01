A few simple tests that show all the things we want to preserve or improve:

# To improve

- Presence of double underscores.

  $ echo "" | $MERLIN single search-by-polarity -query "-float +int64" \
  > -position 1:0 -filename test.ml | \
  > jq '.value.entries[] | del(.info) | del(.kind)'
  {
    "name": "Stdlib__int64.bits_of_float",
    "desc": "float -> int64"
  }
  {
    "name": "Stdlib__int64.of_float",
    "desc": "float -> int64"
  }

- Duplicated elements

  $ echo "" | $MERLIN single search-by-polarity -safe-string \
  > -query "-int +string" -position 1:0 -filename test.ml | \
  > head -n22
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
        {
          "name": "string_of_int",
          "kind": "Value",
          "desc": "int -> string",
          "info": ""
        },

# To keep

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
    "name": "Stdlib__float.add",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.atan2",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.copysign",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.div",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.hypot",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.ldexp",
    "desc": "float -> int -> float"
  }
  {
    "name": "Stdlib__float.mul",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.pow",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.rem",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__float.sub",
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
    "name": "+.",
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
    "name": "atan2",
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
    "name": "ldexp",
    "desc": "float -> int -> float"
  }
  {
    "name": "mod_float",
    "desc": "float -> float -> float"
  }
  {
    "name": "Stdlib__random.State.float",
    "desc": "Stdlib__random.State.t -> float -> float"
  }
