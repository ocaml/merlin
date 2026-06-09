  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

1.) Looking for a function that convert a string to an integer (with
potential failures, so lifting the result in an int option).

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-string +option" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'
  {
    "name": "bool_of_string_opt",
    "desc": "string -> bool option"
  }
  {
    "name": "bool_of_string_opt",
    "desc": "string -> bool option"
  }
  {
    "name": "float_of_string_opt",
    "desc": "string -> float option"
  }
  {
    "name": "float_of_string_opt",
    "desc": "string -> float option"
  }
  {
    "name": "int_of_string_opt",
    "desc": "string -> int option"
  }
  {
    "name": "int_of_string_opt",
    "desc": "string -> int option"
  }
  {
    "name": "Stdlib__Float.of_string_opt",
    "desc": "string -> float option"
  }
  {
    "name": "Stdlib__Int32.of_string_opt",
    "desc": "string -> int32 option"
  }
  {
    "name": "Stdlib__Int64.of_string_opt",
    "desc": "string -> int64 option"
  }
  {
    "name": "Stdlib__Nativeint.of_string_opt",
    "desc": "string -> nativeint option"
  }

2.) Looking for a function that take a list of list of flatten-it into
a list.

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-list +list" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'
  {
    "name": "Stdlib__List.rev",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__List.tl",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.rev",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.tl",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__List.concat",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__List.flatten",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.concat",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.flatten",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__List.cons",
    "desc": "'a -> 'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.cons",
    "desc": "'a -> 'a list -> 'a list"
  }

3.) Looking for a function that take a list and produce a new list
applying a function on every element for the given list (formerly
map).

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-list -list +list" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'
  {
    "name": "Stdlib__List.rev",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__List.tl",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.rev",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.tl",
    "desc": "'a list -> 'a list"
  }
  {
    "name": "Stdlib__List.concat",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__List.flatten",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.concat",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.flatten",
    "desc": "'a list list -> 'a list"
  }
  {
    "name": "Stdlib__List.cons",
    "desc": "'a -> 'a list -> 'a list"
  }
  {
    "name": "Stdlib__ListLabels.cons",
    "desc": "'a -> 'a list -> 'a list"
  }
