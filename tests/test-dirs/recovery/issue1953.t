  $ cat >main.ml <<'EOF'
  > let rec foo x = x + a + b
  > and (a,b) = (1,2)
  > EOF


FIXME Surely we can do better here
  $ $MERLIN single type-enclosing -position 1:16 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | jq
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

FIXME Surely we can do better here
  $ $MERLIN single type-enclosing -position 2:15 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | jq
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
