Start and end should be mandatory

  $ $MERLIN single inlay-hints
  {
    "class": "failure",
    "value": "-start <pos> and -end are mandatory",
    "notifications": []
  }

Start should be mandatory

  $ $MERLIN single inlay-hints -end 1
  {
    "class": "failure",
    "value": "-start <pos> is mandatory",
    "notifications": []
  }

Stop should be mandatory

  $ $MERLIN single inlay-hints -start 1
  {
    "class": "failure",
    "value": "-end <pos> is mandatory",
    "notifications": []
  }
