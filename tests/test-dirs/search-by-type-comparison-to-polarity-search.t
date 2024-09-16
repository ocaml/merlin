  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

1.) Looking for a function that convert a string to an integer (with
potential failures, so lifting the result in an int option).

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "int_of_string_opt",
    "type": "string -> int option"
  }
  {
    "name": "int_of_string_opt",
    "type": "string -> int option"
  }
  {
    "name": "Stdlib__Int32.of_string_opt",
    "type": "string -> int32 option"
  }
  {
    "name": "Stdlib__Int64.of_string_opt",
    "type": "string -> int64 option"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option"
  }
  {
    "name": "Stdlib__Sys.getenv_opt",
    "type": "string -> string option"
  }
  {
    "name": "Stdlib__Float.of_string_opt",
    "type": "string -> float option"
  }

2.) Looking for a function that take a list of list of flatten-it into
a list.


  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list list -> 'a list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "Stdlib__List.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__List.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__Array.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "Stdlib__ArrayLabels.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "Stdlib__Option.join",
    "type": "'a option option -> 'a option"
  }
  {
    "name": "Stdlib__Result.join",
    "type": "(('a, 'e) result, 'e) result -> ('a, 'e) result"
  }
  {
    "name": "Stdlib__Seq.concat",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t"
  }
  {
    "name": "Stdlib__Seq.transpose",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t Stdlib__Seq.t"
  }

3.) Looking for a function that take a list and produce a new list
applying a function on every element for the given list (formerly
map).

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list -> ('a -> 'b) -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "Stdlib__List.map",
    "type": "('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__List.rev_map",
    "type": "('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__ListLabels.map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__ListLabels.rev_map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__List.mapi",
    "type": "(int -> 'a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__ListLabels.mapi",
    "type": "f:(int -> 'a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__List.concat_map",
    "type": "('a -> 'b list) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__List.filter_map",
    "type": "('a -> 'b option) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__ListLabels.concat_map",
    "type": "f:('a -> 'b list) -> 'a list -> 'b list"
  }
  {
    "name": "Stdlib__ListLabels.filter_map",
    "type": "f:('a -> 'b option) -> 'a list -> 'b list"
  }
