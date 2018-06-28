  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "dot_merlins": [],
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }
