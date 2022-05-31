FIXME: the file is not empty
  $ $MERLIN single document -position 2:0 -filename main.ml <main.ml 
  {
    "class": "failure",
    "value": "Empty file",
    "notifications": []
  }

  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  "Empty file"
