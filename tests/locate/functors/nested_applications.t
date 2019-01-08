
  $ $MERLIN single locate -look-for ml -position 17:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/functors/nested_applications.ml",
      "pos": {
        "line": 5,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 19:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/functors/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 2
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 25:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/functors/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 2
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 29:14 \
  > -log-file - -log-section typedtrie \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  # 0.00 typedtrie - functor application
  # 0.00 typedtrie - inspect_functor
  functor application
  # 0.00 typedtrie - inspect_functor_arg
  NOT HANDLED: functor given as functor argument
  # 0.00 typedtrie - inspect_functor
  resolves to Alternative_apply/1021.t[type]
  # 0.00 typedtrie - node
  functor
  # 0.00 typedtrie - node
  functor
  # 0.01 typedtrie - include
  functor application
  # 0.01 typedtrie - inspect_functor
  resolves to Id/1023.t[type]
  {
    "class": "return",
    "value": {
      "file": "tests/locate/functors/nested_applications.ml",
      "pos": {
        "line": 21,
        "col": 54
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 33:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/functors/nested_applications.ml",
      "pos": {
        "line": 5,
        "col": 0
      }
    },
    "notifications": []
  }

