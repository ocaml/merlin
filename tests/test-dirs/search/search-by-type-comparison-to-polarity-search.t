  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

1.) Looking for a function that convert a string to an integer (with
potential failures, so lifting the result in an int option).

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'

2.) Looking for a function that take a list of list of flatten-it into
a list.


  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list list -> 'a list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'

3.) Looking for a function that take a list and produce a new list
applying a function on every element for the given list (formerly
map).

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list -> ('a -> 'b) -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'


4.) Looking for a function that take a list of list of flatten-it into
a list.


  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "'a list list -> 'a list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'

5.) Using polarity query inside search by type (result are a bit
different because type path are a little bit different)

  $ $MERLIN single search-by-type -filename ./main.ml \
  > -position 5:25 -limit 10 -query "-list -list +list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type}'
