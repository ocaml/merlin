type parameter_info =
  { label : Asttypes.arg_label
  ; param_start : int
  ; param_end : int
  ; argument : Typedtree.expression option
  }

type application_signature =
  { fun_name : string option
  ; signature : string
  ; parameters : parameter_info list
  ; active_param : int option
  }

val application_signature :
     prefix:string
  -> Mbrowse.t
  -> [> `Application of application_signature | `Unknown ]
