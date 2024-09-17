  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "string -> int option" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": "Convert the given string to an integer.    The string is read in decimal (by default, or if the string    begins with [0u]), in hexadecimal (if it begins with [0x] or    [0X]), in octal (if it begins with [0o] or [0O]), or in binary    (if it begins with [0b] or [0B]).     The [0u] prefix reads the input as an unsigned integer in the range    [[0, 2*max_int+1]].  If the input exceeds {!max_int}    it is converted to the signed integer    [min_int + input - max_int - 1].     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Return [None] if the given string is not a valid representation of an    integer, or if the integer represented exceeds the range of integers    representable in type [int].    @since 4.05"
  }
  {
    "name": "int_of_string_opt",
    "type": "string -> int option",
    "cost": 0,
    "doc": "Convert the given string to an integer.    The string is read in decimal (by default, or if the string    begins with [0u]), in hexadecimal (if it begins with [0x] or    [0X]), in octal (if it begins with [0o] or [0O]), or in binary    (if it begins with [0b] or [0B]).     The [0u] prefix reads the input as an unsigned integer in the range    [[0, 2*max_int+1]].  If the input exceeds {!max_int}    it is converted to the signed integer    [min_int + input - max_int - 1].     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Return [None] if the given string is not a valid representation of an    integer, or if the integer represented exceeds the range of integers    representable in type [int].    @since 4.05"
  }
  {
    "name": "Int32.of_string_opt",
    "type": "string -> int32 option",
    "cost": 2,
    "doc": "Same as [of_string], but return [None] instead of raising.     @since 4.05"
  }
  {
    "name": "Int64.of_string_opt",
    "type": "string -> int64 option",
    "cost": 2,
    "doc": "Same as [of_string], but return [None] instead of raising.     @since 4.05"
  }
  {
    "name": "Sys.getenv_opt",
    "type": "string -> string option",
    "cost": 4,
    "doc": "Return the value associated to a variable in the process     environment or [None] if the variable is unbound.     @since 4.05"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": "Convert the given string to a boolean.     Return [None] if the string is not [\"true\"] or [\"false\"].    @since 4.05"
  }
  {
    "name": "bool_of_string_opt",
    "type": "string -> bool option",
    "cost": 4,
    "doc": "Convert the given string to a boolean.     Return [None] if the string is not [\"true\"] or [\"false\"].    @since 4.05"
  }
  {
    "name": "Float.of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Same as [of_string], but returns [None] instead of raising."
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Convert the given string to a float.  The string is read in decimal    (by default) or in hexadecimal (marked by [0x] or [0X]).     The format of decimal floating-point numbers is    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.     The format of hexadecimal floating-point numbers is    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an    hexadecimal digit and [d] for a decimal digit.     In both cases, at least one of the integer and fractional parts must be    given; the exponent part is optional.     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Depending on the execution platforms, other representations of    floating-point numbers can be accepted, but should not be relied upon.     Return [None] if the given string is not a valid representation of a float.    @since 4.05"
  }
  {
    "name": "float_of_string_opt",
    "type": "string -> float option",
    "cost": 4,
    "doc": "Convert the given string to a float.  The string is read in decimal    (by default) or in hexadecimal (marked by [0x] or [0X]).     The format of decimal floating-point numbers is    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.     The format of hexadecimal floating-point numbers is    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an    hexadecimal digit and [d] for a decimal digit.     In both cases, at least one of the integer and fractional parts must be    given; the exponent part is optional.     The [_] (underscore) character can appear anywhere in the string    and is ignored.     Depending on the execution platforms, other representations of    floating-point numbers can be accepted, but should not be relied upon.     Return [None] if the given string is not a valid representation of a float.    @since 4.05"
  }


  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 5:25 -limit 10 -query "('a -> 'b) -> 'a list -> 'b list" |
  > tr '\n' ' ' | jq  '.value[] | {name,type,cost,doc}'
  {
    "name": "List.map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": "[map f [a1; ...; an]] applies function [f] to [a1, ..., an],    and builds the list [[f a1; ...; f an]]    with the results returned by [f]."
  }
  {
    "name": "List.rev_map",
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": "[rev_map f l] gives the same result as    {!rev}[ (]{!map}[ f l)], but is more efficient."
  }
  {
    "name": "ListLabels.map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": "[map ~f [a1; ...; an]] applies function [f] to [a1, ..., an],    and builds the list [[f a1; ...; f an]]    with the results returned by [f]."
  }
  {
    "name": "ListLabels.rev_map",
    "type": "f:('a -> 'b) -> 'a list -> 'b list",
    "cost": 0,
    "doc": "[rev_map ~f l] gives the same result as    {!rev}[ (]{!map}[ f l)], but is more efficient."
  }
  {
    "name": "List.mapi",
    "type": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5,
    "doc": "Same as {!map}, but the function is applied to the index of    the element as first argument (counting from 0), and the element    itself as second argument.    @since 4.00"
  }
  {
    "name": "ListLabels.mapi",
    "type": "f:(int -> 'a -> 'b) -> 'a list -> 'b list",
    "cost": 5,
    "doc": "Same as {!map}, but the function is applied to the index of    the element as first argument (counting from 0), and the element    itself as second argument.    @since 4.00"
  }
  {
    "name": "List.concat_map",
    "type": "('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10,
    "doc": "[concat_map f l] gives the same result as     {!concat}[ (]{!map}[ f l)]. Tail-recursive.     @since 4.10"
  }
  {
    "name": "List.filter_map",
    "type": "('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10,
    "doc": "[filter_map f l] applies [f] to every element of [l], filters     out the [None] elements and returns the list of the arguments of     the [Some] elements.     @since 4.08"
  }
  {
    "name": "ListLabels.concat_map",
    "type": "f:('a -> 'b list) -> 'a list -> 'b list",
    "cost": 10,
    "doc": "[concat_map ~f l] gives the same result as     {!concat}[ (]{!map}[ f l)]. Tail-recursive.     @since 4.10"
  }
  {
    "name": "ListLabels.filter_map",
    "type": "f:('a -> 'b option) -> 'a list -> 'b list",
    "cost": 10,
    "doc": "[filter_map ~f l] applies [f] to every element of [l], filters     out the [None] elements and returns the list of the arguments of     the [Some] elements.     @since 4.08"
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
        "name": "Hashtbl.add",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 33,
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
        "name": "Hashtbl.replace",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> 'a -> 'b -> unit",
        "cost": 34,
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
        "name": "Hashtbl.add_seq",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 46,
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
        "name": "Hashtbl.replace_seq",
        "type": "('a, 'b) Stdlib__Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 47,
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
        "name": "MoreLabels.Hashtbl.add_seq",
        "type": "('a, 'b) Stdlib__MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 48,
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
        "name": "MoreLabels.Hashtbl.replace_seq",
        "type": "('a, 'b) Stdlib__MoreLabels.Hashtbl.t -> ('a * 'b) Seq.t -> unit",
        "cost": 49,
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
        "name": "Result.bind",
        "type": "('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result",
        "cost": 61,
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
        "name": "Either.map",
        "type": "left:('a1 -> 'a2) ->
  right:('b1 -> 'b2) ->
  ('a1, 'b1) Stdlib__Either.t -> ('a2, 'b2) Stdlib__Either.t",
        "cost": 77,
        "doc": "[map ~left ~right (Left v)] is [Left (left v)],
      [map ~left ~right (Right v)] is [Right (right v)]."
      }
    ],
    "notifications": []
  }
