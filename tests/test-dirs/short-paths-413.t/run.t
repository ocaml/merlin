
FIXME WHEN SHORTPATH WILL BE AVALABLE AGAIN
  $ $MERLIN single type-enclosing -principal -short-paths \
  > -position 2:49 -filename test.ml << EOF
  > type module_declaration_lazy = int
  > and module_data = { mda_declaration : module_declaration_lazy;}
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 38
        },
        "end": {
          "line": 2,
          "col": 61
        },
        "type": "type module_declaration_lazy = module_declaration_lazy",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 38
        },
        "end": {
          "line": 2,
          "col": 61
        },
        "type": "module_declaration_lazy",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 38
        },
        "end": {
          "line": 2,
          "col": 61
        },
        "type": "module_declaration_lazy",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 63
        },
        "type": "type module_data = { mda_declaration : module_declaration_lazy; }",
        "tail": "no"
      }
    ],
    "notifications": []
  }
