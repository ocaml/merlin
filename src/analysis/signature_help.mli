type application_signature =
  { fun_name : string option
  ; signature : string
  ; param_offsets : (int * int) list
  ; active_param : int option
  }

val application_signature
   : Mbrowse.t
  -> [> `Application of application_signature | `Unknown ]
