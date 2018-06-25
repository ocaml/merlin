open CamlinternalFormatBasics
open CamlinternalFormat

let ikfprintf k oc (Format (fmt, _)) =
  make_printf (fun oc _ -> k oc) oc End_of_acc fmt
