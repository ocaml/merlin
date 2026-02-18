(* This forward reference is filled in Lexer_raw.mll *)
let is_keyword_ref : (string -> bool) ref = ref (fun _ -> false)
let is_keyword txt = !is_keyword_ref txt
