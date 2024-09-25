type parameter_info =
  { label : Asttypes.arg_label;
    param_start : int;
    param_end : int;
    argument : Typedtree.expression option
  }

type application_signature =
  { function_name : string option;
    function_position : Msource.position;
    signature : string;
    parameters : parameter_info list;
    active_param : int option
  }

(** provide signature information for applied functions *)
val application_signature :
  prefix:string ->
  cursor:Lexing.position ->
  Mbrowse.t ->
  application_signature option

(** @see <https://ocaml.org/manual/lex.html> reference *)
val prefix_of_position :
  short_path:bool -> Msource.t -> Msource.position -> string
