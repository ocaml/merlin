  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

1.) Looking for a function that convert a string to an integer (with
potential failures, so lifting the result in an int option).

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-string +option" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'

2.) Looking for a function that take a list of list of flatten-it into
a list.

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-list +list" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'

3.) Looking for a function that take a list and produce a new list
applying a function on every element for the given list (formerly
map).

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-list -list +list" |
  > tr '\n' ' ' | jq  '.value.entries[:10][] | {name,desc}'
