

  $ $MERLIN server stop-server

Let's start with populating typer cache
  $ echo "let x = 1 \
    \
    module M = struct \
    let y = 2 \
    let z = 3 \
    end" > before.ml

  $ $MERLIN server errors -filename test.ml < before.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

TODO: After change inside module, Merlin should reuse items that are in module's prefix.
  $ echo "let x = 1 \
    \
    module M = struct \
    let y = 2 \
    let a = 4 \
    let z = 3 \
    end" > after.ml

  $ $MERLIN server errors -filename test.ml < after.ml \
  > -log-section Mtyper -log-file - 2>&1
  # 0.01 Mtyper - compatible_prefix
  reusing 1 items, 1 new items to type
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
