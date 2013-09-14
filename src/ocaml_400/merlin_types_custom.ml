let include_idents l = l

let lookup_constructor id env = snd (Env.lookup_constructor id env)
let lookup_label id env = snd (Env.lookup_label id env)
let fold_types = Env.fold_types

let fold_constructors f =
  Env.fold_constructors (fun name _ descr -> f name descr)
let fold_labels f = Env.fold_labels (fun _ _ -> f)
