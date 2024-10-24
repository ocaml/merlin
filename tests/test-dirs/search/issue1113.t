  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-ezfnifzen +ezfzef" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'
  {
    "name": "CamlinternalOO.dummy_table",
    "desc": "CamlinternalOO.table"
  }
  {
    "name": "CamlinternalOO.params",
    "desc": "CamlinternalOO.params"
  }
  {
    "name": "__FILE__",
    "desc": "string"
  }
  {
    "name": "__FILE__",
    "desc": "string"
  }
  {
    "name": "__FUNCTION__",
    "desc": "string"
  }
  {
    "name": "__FUNCTION__",
    "desc": "string"
  }
  {
    "name": "__LINE__",
    "desc": "int"
  }
  {
    "name": "__LINE__",
    "desc": "int"
  }
  {
    "name": "__LOC__",
    "desc": "string"
  }
  {
    "name": "__LOC__",
    "desc": "string"
  }
