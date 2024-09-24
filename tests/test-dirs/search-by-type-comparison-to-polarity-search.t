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
    "name": "Int32.of_string_opt",
    "type": "string -> int32 option"
  }
  {
    "name": "Int64.of_string_opt",
    "type": "string -> int64 option"
  }
  {
    "name": "Sys.getenv_opt",
    "type": "string -> string option"
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
    "name": "Float.of_string_opt",
    "type": "string -> float option"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option"
  }

2.) Looking for a function that take a list of list of flatten-it into
a list.


  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list list -> 'a list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "List.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "List.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Array.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "ArrayLabels.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "Seq.concat",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t"
  }
  {
    "name": "Option.join",
    "type": "'a option option -> 'a option"
  }
  {
    "name": "Seq.transpose",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t Stdlib__Seq.t"
  }
  {
    "name": "Result.join",
    "type": "(('a, 'e) result, 'e) result -> ('a, 'e) result"
  }

3.) Looking for a function that take a list and produce a new list
applying a function on every element for the given list (formerly
map).

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list -> ('a -> 'b) -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "List.map",
    "type": "('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "List.rev_map",
    "type": "('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "ListLabels.map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "ListLabels.rev_map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "List.mapi",
    "type": "(int -> 'a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "ListLabels.mapi",
    "type": "f:(int -> 'a -> 'b) -> 'a list -> 'b list"
  }
  {
    "name": "Seq.map",
    "type": "('a -> 'b) -> 'a Stdlib__Seq.t -> 'b Stdlib__Seq.t"
  }
  {
    "name": "List.concat_map",
    "type": "('a -> 'b list) -> 'a list -> 'b list"
  }
  {
    "name": "List.filter_map",
    "type": "('a -> 'b option) -> 'a list -> 'b list"
  }
  {
    "name": "ListLabels.concat_map",
    "type": "f:('a -> 'b list) -> 'a list -> 'b list"
  }


4.) Looking for a function that take a list of list of flatten-it into
a list.


  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list list -> 'a list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "List.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "List.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "Array.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "ArrayLabels.concat",
    "type": "'a array list -> 'a array"
  }
  {
    "name": "Seq.concat",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t"
  }
  {
    "name": "Option.join",
    "type": "'a option option -> 'a option"
  }
  {
    "name": "Seq.transpose",
    "type": "'a Stdlib__Seq.t Stdlib__Seq.t -> 'a Stdlib__Seq.t Stdlib__Seq.t"
  }
  {
    "name": "Result.join",
    "type": "(('a, 'e) result, 'e) result -> ('a, 'e) result"
  }

5.) Using polarity query inside search by type (result are a bit
different because type path are a little bit different)

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "-list -list +list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
  {
    "name": "List.tl",
    "type": "'a list -> 'a list"
  }
  {
    "name": "List.rev",
    "type": "'a list -> 'a list"
  }
  {
    "name": "ListLabels.tl",
    "type": "'a list -> 'a list"
  }
  {
    "name": "ListLabels.rev",
    "type": "'a list -> 'a list"
  }
  {
    "name": "List.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "List.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.concat",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "ListLabels.flatten",
    "type": "'a list list -> 'a list"
  }
  {
    "name": "List.cons",
    "type": "'a -> 'a list -> 'a list"
  }
  {
    "name": "ListLabels.cons",
    "type": "'a -> 'a list -> 'a list"
  }
