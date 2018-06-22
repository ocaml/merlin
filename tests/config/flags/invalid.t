  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "dot_merlins": [
        ".merlin"
      ],
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }
