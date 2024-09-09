  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > jq '.value[] | {name,type,cost}'
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0
  }
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0
  }
  {
    "name": "Stdlib__Int32.of_string_opt",
    "type": "string -> int32 option",
    "cost": 2
  }
  {
    "name": "Stdlib__Int64.of_string_opt",
    "type": "string -> int64 option",
    "cost": 2
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4
  }
  {
    "name": "Stdlib__Sys.getenv_opt",
    "type": "string -> string option",
    "cost": 4
  }
  {
    "name": "Stdlib__Float.of_string_opt",
    "type": "string -> float option",
    "cost": 4
  }


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "('a -> 'b) -> 'a list -> 'b list" |
  > jq '.value[] | {name,type,cost}'
  {
    "name": "Stdlib__List.map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0
  }
  {
    "name": "Stdlib__List.rev_map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0
  }
  {
    "name": "Stdlib__ListLabels.map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0
  }
  {
    "name": "Stdlib__ListLabels.rev_map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0
  }
  {
    "name": "Stdlib__List.mapi",
    "type": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5
  }
  {
    "name": "Stdlib__ListLabels.mapi",
    "type": "f:(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5
  }
  {
    "name": "Stdlib__List.filter_map",
    "type": "('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10
  }
  {
    "name": "Stdlib__List.concat_map",
    "type": "('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10
  }
  {
    "name": "Stdlib__ListLabels.filter_map",
    "type": "f:('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10
  }
  {
    "name": "Stdlib__ListLabels.concat_map",
    "type": "f:('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10
  }

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
        "cost": 35
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
        "cost": 36
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
        "cost": 48
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
        "cost": 49
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
        "cost": 50
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
        "cost": 51
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
        "cost": 63
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
        "cost": 68
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
        "cost": 68
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
        "cost": 79
      }
    ],
    "notifications": []
  }
