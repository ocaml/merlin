# Polarity search

A Hoogle-like type-based search for libraries that are in merlin's scope.

The commands `:MerlinSearch` (vim) / `merlin-search` (emacs) take a search query
and return the list of identifiers that satisfy this query.

The query language is simply a list of path identifiers prefixed by `+` or `-`,
e.g. `-int`, `-int +string`, `-Hashtbl.t +int`.

`-` is interpreted as "consuming" and `+` as "producing": `-int +string` looks
for functions consuming an `int` and producing a `string`.

The search algorithm uses type variance to filter results. Thus, search will
proceed inside abstract types, continuation-passing-style, ... as long as
variance annotations are available.

# Open refactoring

Merlin provides a pair of commands to help cleaning the code in the scope of an
`open` statement.

Two new commands (`:MerlinRefactorOpen`, `:MerlinRefactorOpenQualify` in vim,
and `merlin-refactor-open`, `merlin-refactor-open-qualify` in Emacs) help
cleaning the code in the scope of an `open` statement.

When the cursor is on an open statement:
- `:MerlinRefactorOpen` (vim) / `merlin-refactor-open` (emacs) will remove
  references to the path of the open that are made useless
- `:MerlinRefactorOpenQualify` (vim) / `merlin-refactor-open-qualify` (emacs)
  will always add references to this path

Starting from:

```ocaml
open Unix

let times = Unix.times ()
let f x = x.Unix.tms_stime, x.Unix.tms_utime
```

Calling `:MerlinRefactorOpen` with the cursor on the open statement will
produce:

```ocaml
open Unix

let times = times ()
let f x = x.tms_stime, x.tms_utime
```

Calling `:MerlinRefactorOpenQualify` will restore:

```ocaml
open Unix

let times = Unix.times ()
let f x = x.Unix.tms_stime, x.Unix.tms_utime
```
