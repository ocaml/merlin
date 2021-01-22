Default completion:

  $ $MERLIN single complete-prefix -position 3:10 -filename test.ml \
  > -prefix fu < test.ml| jq ".value.entries[].name"
  "funnyny"

Keywords only:

  $ $MERLIN single complete-prefix -position 3:10 -filename test.ml \
  > -kind k -prefix fu < test.ml| jq ".value.entries[].name"
  "function"
  "fun"
  "functor"

Keywords and values:

  $ $MERLIN single complete-prefix -position 3:10 -filename test.ml \
  > -kind keyword -kind value -prefix fu < test.ml| jq ".value.entries[].name"
  "funnyny"
  "function"
  "fun"
  "functor"

Keywords only including extension:

  $ echo "f" | $MERLIN single complete-prefix -position 1:2 -filename test.ml \
  > -kind k -prefix f -extension lwt | jq ".value.entries[].name"
  "finally"
  "for_lwt"
  "function"
  "false"
  "fun"
  "for"
  "functor"

And let's also make sure we don't offer keywords when we completing under a
certain path

  $ $MERLIN single complete-prefix -position 5:14 -filename test.ml \
  > -prefix List.f < test.ml| jq ".value.entries[].name"
  "fast_sort"
  "filter"
  "filter_map"
  "filteri"
  "find"
  "find_all"
  "find_map"
  "find_opt"
  "flatten"
  "fold_left"
  "fold_left2"
  "fold_left_map"
  "fold_right"
  "fold_right2"
  "for_all"
  "for_all2"

  $ $MERLIN single complete-prefix -position 5:14 -filename test.ml \
  > -kind k -prefix List.f < test.ml| jq ".value.entries"
  []
