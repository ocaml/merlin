
let () =
  match Lib.v with
  | { Lib.B.label_label = 3 } -> ()
  | { Lib.B.label_label } -> ignore label_label

