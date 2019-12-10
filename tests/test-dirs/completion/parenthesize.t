  $ $MERLIN single complete-prefix -position 11:15 -prefix MyList. \
  > -filename parenthesize.ml < parenthesize.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "()",
      "kind": "Constructor",
      "desc": "MyList.u",
      "info": ""
    },
    {
      "name": "(::)",
      "kind": "Constructor",
      "desc": "'a * 'a MyList.t -> 'a MyList.t",
      "info": ""
    },
    {
      "name": "(mod)",
      "kind": "Value",
      "desc": "MyList.u",
      "info": ""
    },
    {
      "name": "[]",
      "kind": "Constructor",
      "desc": "'a MyList.t",
      "info": ""
    },
    {
      "name": "random",
      "kind": "Value",
      "desc": "int",
      "info": ""
    },
    {
      "name": "t",
      "kind": "Type",
      "desc": "type 'a t = (::) of 'a * 'a MyList.t | []",
      "info": ""
    },
    {
      "name": "u",
      "kind": "Type",
      "desc": "type u = ()",
      "info": ""
    }
  ]
