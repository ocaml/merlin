  $ $MERLIN single errors -filename foo.ml < foo.ml
  {
    "class": "failure",
    "value": "Graph.add: type already defined: t/274[1]",
    "notifications": []
  }

FIXME (appears undeterministic)
$ $MERLIN single errors -filename nasty.ml < nasty.ml
