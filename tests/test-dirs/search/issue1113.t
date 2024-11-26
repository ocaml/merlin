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
    "name": "Dynlink.is_native",
    "desc": "bool"
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

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "ezfnifzen -> ezfzef" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost}'
  {
    "name": "Gc.major",
    "type": "unit -> unit",
    "cost": 13
  }
  {
    "name": "Gc.minor",
    "type": "unit -> unit",
    "cost": 13
  }
  {
    "name": "Sys.time",
    "type": "unit -> float",
    "cost": 13
  }
  {
    "name": "read_int",
    "type": "unit -> int",
    "cost": 13
  }
  {
    "name": "read_int",
    "type": "unit -> int",
    "cost": 13
  }
  {
    "name": "Unix.fork",
    "type": "unit -> int",
    "cost": 13
  }
  {
    "name": "Unix.time",
    "type": "unit -> float",
    "cost": 13
  }
  {
    "name": "flush_all",
    "type": "unit -> unit",
    "cost": 13
  }
  {
    "name": "flush_all",
    "type": "unit -> unit",
    "cost": 13
  }
  {
    "name": "read_line",
    "type": "unit -> string",
    "cost": 13
  }
