  $ $MERLIN single complete-prefix -kind keyword -position 1:3 -filename kw.ml -prefix fu <<EOF \
  > fu
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "fun",
          "kind": "Keyword",
          "desc": "",
          "info": "",
          "deprecated": false
        },
        {
          "name": "function",
          "kind": "Keyword",
          "desc": "",
          "info": "",
          "deprecated": false
        },
        {
          "name": "functor",
          "kind": "Keyword",
          "desc": "",
          "info": "",
          "deprecated": false
        }
      ],
      "context": [
        "application",
        {
          "argument_type": "'_weak1",
          "labels": []
        }
      ]
    },
    "notifications": []
  }
