These tests ensure the stability of identifier reconstruction
in the presence of underscores.

1.1
  $ $MERLIN single type-enclosing -position 3:2 -filename under.ml <<EOF | \
  > jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

1.2
  $ $MERLIN single type-enclosing -position 3:3 -filename under.ml <<EOF | \
  >  jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 6
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

We try several places in the identifier to check the result stability
2.1
  $ $MERLIN single type-enclosing -position 3:5 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.2
  $ $MERLIN single type-enclosing -position 3:6 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.3
  $ $MERLIN single type-enclosing -position 3:7 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 6
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.4
  $ $MERLIN single type-enclosing -position 3:8 -filename under.ml <<EOF
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

3.1
FIXME UPGRADE 5.2: there is something wrong with the recovery here
  $ $MERLIN single type-enclosing -position 5:10 -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int option",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "'a -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": "[
    structure_item (under.ml[1,0+0]..under.ml[1,0+12])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[1,0+4]..under.ml[1,0+6])
            Tpat_var \"aa/276\"
          expression (under.ml[1,0+9]..under.ml[1,0+12])
            Texp_constant Const_float 4.2
      ]
    structure_item (under.ml[2,13+0]..under.ml[5,70+17])
      Tstr_value Nonrec
      [
        <def>
          pattern (under.ml[2,13+4]..under.ml[2,13+5])
            Tpat_var \"f/277\"
          expression (under.ml[2,13+6]..under.ml[5,70+17]) ghost
            Texp_function
            [
              Nolabel
              Param_pat
                pattern (under.ml[2,13+6]..under.ml[2,13+9])
                  Tpat_var \"x/279\"
            ]
            Tfunction_cases (under.ml[2,13+18]..under.ml[5,70+17])
              Texp_constraint
              core_type (under.ml[2,13+12]..under.ml[2,13+15])
                Ttyp_constr \"int/1!\"
                []
              [
                <case>
                  pattern (under.ml[3,40+4]..under.ml[3,40+8])
                    Tpat_construct \"None\"
                    []
                    None
                  expression (under.ml[3,40+12]..under.ml[3,40+13])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 3
                <case>
                  pattern (under.ml[4,54+4]..under.ml[4,54+10])
                    Tpat_construct \"Some\"
                    [
                      pattern (under.ml[4,54+9]..under.ml[4,54+10])
                        Tpat_constant Const_int 5
                    ]
                    None
                  expression (under.ml[4,54+14]..under.ml[4,54+15])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 4
                <case>
                  pattern (under.ml[5,70+4]..under.ml[5,70+12])
                    Tpat_construct \"Some\"
                    [
                      pattern (under.ml[5,70+9]..under.ml[5,70+12])
                        Tpat_var \"_aa/280\"
                    ]
                    None
                  expression (under.ml[5,70+16]..under.ml[5,70+17])
                    attribute \"merlin.loc\"
                      []
                    Texp_constant Const_int 4
              ]
      ]
  ]
  
  
  ",
    "notifications": []
  }
