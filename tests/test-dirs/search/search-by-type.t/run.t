  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "('a -> 'b) -> 'a list -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'

  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 \
  > -query "Hashtbl : ('f, 'g) Hashtbl.t -> 'f -> 'g -> unit"
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -with-doc true -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
