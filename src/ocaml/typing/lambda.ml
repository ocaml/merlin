(* The lambda representation is of no interest for Merlin, but some types are
   used by [value_rec_check]. *)

type immediate_or_pointer =
  | Immediate
  | Pointer

type array_kind =
  Pgenarray | Paddrarray | Pintarray | Pfloatarray
