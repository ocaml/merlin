  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries client))
  > 
  > (rule
  >  (targets client.ml)
  > ; (mode (promote (until-clean)))
  >  (action (write-file %{targets} "let x = 42")))
  > 
  > (library
  >  (name client)
  >  (modules client)
  >  (wrapped false))
  > EOF

  $ dune exec ./main.exe
  Hello, World: 42

  $ cat _build/default/client.ml
  let x = 42

Locate fails to find the source if it is not promoted:
  $ $MERLIN single locate -look-for ml -position 1:52 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "'Client.x' seems to originate from 'Client' whose ML file could not be found",
    "notifications": []
  }

A solution is to promote the generated file to the source tree:
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries client))
  > 
  > (rule
  >  (targets client.ml)
  >  (mode (promote (until-clean)))
  >  (action (write-file %{targets} "let x = 42")))
  > 
  > (library
  >  (name client)
  >  (modules client)
  >  (wrapped false))
  > EOF

  $ dune build ./main.exe

  $ $MERLIN single locate -look-for ml -position 1:52 -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/client.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }
