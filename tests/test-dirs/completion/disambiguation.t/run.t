Completing out-of-scope constructor names when the type information is
available:

  $ $MERLIN single complete-prefix -position 5:18 -prefix Foo -doc n \
  > -filename constr.ml < constr.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "Foobar",
          "kind": "Constructor",
          "desc": "T.t",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

Try completing field names inside record expressions (where either the scope or
the type should be known):

  $ $MERLIN single complete-prefix -position 5:17 -prefix T.f -doc n \
  > -filename record.ml < record.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }


  $ $MERLIN single complete-prefix -position 7:18 -prefix foo -doc n \
  > -filename record.ml < record.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

  $ $MERLIN single complete-prefix -position 11:31 -prefix tes -doc n \
  > -filename record.ml < record.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "test_other",
          "kind": "Label",
          "desc": "T.t -> float",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }
