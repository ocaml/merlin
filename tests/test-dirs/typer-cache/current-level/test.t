The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server

Then we can look at the current level and whether it's being reset between
buffers, and different runs for the same buffer:

  $ echo "let f x = x" | \
  > $MERLIN server dump -what current-level -filename test.ml
  {
    "class": "return",
    "value": 0,
    "notifications": []
  }

  $ echo "let f x = x" | \
  > $MERLIN server dump -what current-level -filename test.ml
  {
    "class": "return",
    "value": 0,
    "notifications": []
  }

  $ echo "type u= Uouo let f x = x type t = Toto" | \
  > $MERLIN server dump -what current-level -filename test.ml
  {
    "class": "return",
    "value": 2,
    "notifications": []
  }

  $ echo "let f x = x" | \
  > $MERLIN server dump -what current-level -filename test.ml
  {
    "class": "return",
    "value": 2,
    "notifications": []
  }


  $ echo "let f x = x" | \
  > $MERLIN server dump -what current-level -filename other_test.ml
  {
    "class": "return",
    "value": 0,
    "notifications": []
  }


  $ echo "let f x = x" | \
  > $MERLIN server dump -what current-level -filename test.ml
  {
    "class": "return",
    "value": 2,
    "notifications": []
  }

