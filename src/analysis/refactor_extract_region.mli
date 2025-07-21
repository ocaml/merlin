(** Region extractor allows extracting arbitrary expression into a fresh
    toplevel binding. The extractor detects bounded variables inside the
    extracted expression and performs code generation acordingly.

    For instance, let's assume that we want to extract the pi value oustide of
    the body of [circle_area]:
    {[
      let circle_area radius = 3.14159 *. (radius ** 2.)
                            (* ^^^^^^^ *)
    ]}

    The generated code will look like this:
    {[
      let const_name1 = 3.14159
      let circle_area radius = const_name1 *. (radius ** 2.)
    ]}

    Extraction also works on expressions that are functions:
    {[
      let all_empty l =
        List.for_all
          (function
            | [] -> true
            | _ -> false)
       (* ^^^^^^^^^^^^^^^ *)
          l
    ]}

    {[
      let is_empty = (function | [] -> true | _ -> false)
      let all_empty l = List.for_all is_empty l
    ]}

    Let's look at a more complicated example where we want to extract the entire
    body of [f]:
    {[
      let rec f x = 10 + y + x
                 (* ^^^^^^^^^^ *)

      and y = 80
    ]}

    Performing the extraction leads to this code:
    {[
      let rec f x = fun_name2 x

      and y = 80

      and fun_name2 x = 10 + y + x
    ]}

    We can see that extractor detects this kind of pattern and extracts the
    expression inside an [and] binding. It also substitutes the expression by a
    call to the fresh generated function with the correct parameters.

    Finally, if there is no bounded variable in the expression, a trailing unit
    parameter is added to the generated let binding in order to preserve the
    evaluation order. Let's extract the entire body of [x]:
    {[
      let my_list =
        print_endline "Wild side effect!";
        1 :: [ 2; 3; 4 ]
    ]}

    {[
      let fun_name1 () =
        print_endline "Wild side effect!";
        [ 1; 2; 3; 4 ]
      let f = fun_name1 ()
    ]}

    Final remarks:
    - Extraction currently works on any typedtree expression that doesn't have a
      ghost location. This restriction prevents the generation of invalid code.

    - The generated code is pretty printed by the compiler libs and may not be
      formatted according to OCamlformat conventions.

*)

(** Raised when extractor is not ables to select an expression to extract in
    given location interval. *)
exception Nothing_to_do

(** Raised when extraction is called inside an interface file. *)
exception Not_allowed_in_interface_file

(** Is an expression is extractable in the given region? *)
val is_region_extractable :
  start:Lexing.position ->
  stop:Lexing.position ->
  (Env.t * Browse_raw.node) list ->
  Typedtree.structure ->
  bool

(** [substitute ~start ~stop ~extract_name buffer typedtree] tries to
    extract the most inclusive expression located in interval [start-stop] into
    a fresh toplevel generated let binding.

    Returns a {!Query_protocol.substitution_result} consisting of three fields:
    - [loc]: the location where [content] musts be inserted.
    - [content]: the code where the substitution takes places completed by the
      generated let binding.
    - [selection_range]: the location where to position the cursor for easy
      renaming of the generated let binding.

    If there is no [extract_name] provided, the generated binding is named with
    an untaken name in its current scope. Extracted constants will be named with
    a name beginning with ["const_name"], while extracted functions will have a
    name beginning with ["fun_name"]. *)
val substitute :
  start:Lexing.position ->
  stop:Lexing.position ->
  ?extract_name:string ->
  Msource.t ->
  Mtyper.typedtree ->
  Query_protocol.substitution_result
