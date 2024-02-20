**************************
When building without Dune
**************************

  $ ocamlc -c -bin-annot anothermod.mli 
  $ ocamlc -c -bin-annot anothermod.ml 


  $ cat >.merlin << EOF
  > EOF

Jump from to another module `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for ml -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump from to another module signature `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for mli -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to an element of an aliased module `A.|f`:
  $ $MERLIN single locate -look-for ml -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump to the declaration of an element of an alisaed module `A.|f`:
  $ $MERLIN single locate -look-for mli -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump to an aliased module `A|.f`:
  $ $MERLIN single locate -look-for ml -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to the declaration of an aliased module `A|.f`.
The alias is traversed.
  $ $MERLIN single locate -look-for mli -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

***********************
When building with Dune
***********************

With Dune we have an additional issue: the automatic wrapping
  $ rm .merlin
  $ rm *.cm*

We need to build @check for all cmts to be created
  $ dune build @check
  $ dune build

Jump from to another module `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for ml -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump from to another module signature `module A = Anothe|rmod`:
  $ $MERLIN single locate -look-for mli -position 1:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to an element of an aliased module `A.|f`:
  $ $MERLIN single locate -look-for ml -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump from to another module value decl `Anothermod.|a`:
  $ $MERLIN single locate -look-for mli -position 3:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 2,
      "col": 5
    }
  }

Jump from to another module value def `Anothermod.|a`:
  $ $MERLIN single locate -look-for ml -position 3:21 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 2,
      "col": 5
    }
  }


Jump to the declaration of an element of an alisaed module `A.|f`:
  $ $MERLIN single locate -look-for mli -position 5:7 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 3,
      "col": 4
    }
  }

Jump to an aliased module `A|.f`:
  $ $MERLIN single locate -look-for ml -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to the declaration of an aliased module `A|.f`:
The alias is traversed.
  $ $MERLIN single locate -look-for mli -position 5:2 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/anothermod.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }
