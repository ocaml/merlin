  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > jq '.value[] | {name,type,cost}'
  jq: parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 34, column 15
  [5]


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "('a -> 'b) -> 'a list -> 'b list" |
  > jq '.value[] | {name,type,cost}'
  jq: parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 19, column 37
  [5]

  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 \
  > -query "Hashtbl : ('f, 'g) Hashtbl.t -> 'f -> 'g -> unit"
  {
    "class": "return",
    "value": [
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 116,
          "col": 0
        },
        "end": {
          "line": 116,
          "col": 40
        },
        "name": "Stdlib__Hashtbl.add",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 35,
        "doc": "[Hashtbl.add tbl key data] adds a binding of [key] to [data]
     in table [tbl].
  
     {b Warning}: Previous bindings for [key] are not removed, but simply
     hidden. That is, after performing {!remove}[ tbl key],
     the previous binding for [key], if any, is restored.
     (Same behavior as with association lists.)
  
     If you desire the classic behavior of replacing elements,
     see {!replace}."
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 151,
          "col": 0
        },
        "end": {
          "line": 151,
          "col": 44
        },
        "name": "Stdlib__Hashtbl.replace",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 36,
        "doc": "[Hashtbl.replace tbl key data] replaces the current binding of [key]
     in [tbl] by a binding of [key] to [data].  If [key] is unbound in [tbl],
     a binding of [key] to [data] is added to [tbl].
     This is functionally equivalent to {!remove}[ tbl key]
     followed by {!add}[ tbl key data]."
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 301,
          "col": 0
        },
        "end": {
          "line": 301,
          "col": 50
        },
        "name": "Stdlib__Hashtbl.add_seq",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 48,
        "doc": "Add the given bindings to the table, using {!add}
      @since 4.07"
      },
      {
        "file": "hashtbl.mli",
        "start": {
          "line": 305,
          "col": 0
        },
        "end": {
          "line": 305,
          "col": 54
        },
        "name": "Stdlib__Hashtbl.replace_seq",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 49,
        "doc": "Add the given bindings to the table, using {!replace}
      @since 4.07"
      },
      {
        "file": "moreLabels.mli",
        "start": {
          "line": 318,
          "col": 2
        },
        "end": {
          "line": 318,
          "col": 52
        },
        "name": "Stdlib__MoreLabels.Hashtbl.add_seq",
        "type": "('a, 'b) Stdlib__MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 50,
        "doc": "Add the given bindings to the table, using {!add}
        @since 4.07"
      },
      {
        "file": "moreLabels.mli",
        "start": {
          "line": 322,
          "col": 2
        },
        "end": {
          "line": 322,
          "col": 56
        },
        "name": "Stdlib__MoreLabels.Hashtbl.replace_seq",
        "type": "('a, 'b) Stdlib__MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 51,
        "doc": "Add the given bindings to the table, using {!replace}
        @since 4.07"
      },
      {
        "file": "result.mli",
        "start": {
          "line": 47,
          "col": 0
        },
        "end": {
          "line": 47,
          "col": 72
        },
        "name": "Stdlib__Result.bind",
        "type": "('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result",
        "cost": 63,
        "doc": "[bind r f] is [f v] if [r] is [Ok v] and [r] if [r] is [Error _]."
      },
      {
        "file": "stdlib.mli",
        "start": {
          "line": 1324,
          "col": 0
        },
        "end": {
          "line": 1324,
          "col": 65
        },
        "name": "string_of_format",
        "type": "('a, 'b, 'c, 'd, 'e, 'f) format6 -> string",
        "cost": 68,
        "doc": "Converts a format string into a string."
      },
      {
        "file": "stdlib.mli",
        "start": {
          "line": 1324,
          "col": 0
        },
        "end": {
          "line": 1324,
          "col": 65
        },
        "name": "string_of_format",
        "type": "('a, 'b, 'c, 'd, 'e, 'f) format6 -> string",
        "cost": 68,
        "doc": "Converts a format string into a string."
      },
      {
        "file": "either.mli",
        "start": {
          "line": 86,
          "col": 0
        },
        "end": {
          "line": 87,
          "col": 73
        },
        "name": "Stdlib__Either.map",
        "type": "left:('a1 -> 'a2) ->
  right:('b1 -> 'b2) ->
  ('a1, 'b1) Stdlib__Either.t -> ('a2, 'b2) Stdlib__Either.t",
        "cost": 79,
        "doc": "[map ~left ~right (Left v)] is [Left (left v)],
      [map ~left ~right (Right v)] is [Right (right v)]."
      }
    ],
    "notifications": []
  }
