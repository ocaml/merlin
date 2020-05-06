  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "dot_merlins": [
        "tests/dune"
      ],
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }
