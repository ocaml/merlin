type info = 
{ 
    name : string; 
    description : string; 
    documentation : string 
}

val get_syntax_doc: (Env.t * Browse_raw.node) list -> [> `Found of string | `No_documentation]
