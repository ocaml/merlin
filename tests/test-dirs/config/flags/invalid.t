  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "dot_merlins": [
        "$TESTCASE_ROOT/.merlin"
      ],
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }
