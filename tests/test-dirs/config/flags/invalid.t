  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }
