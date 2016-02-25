open Utils

module type S = sig
  module G : G

  val cost_of_prod    : G.production -> float
  val penalty_of_item : G.production * int -> float
  val cost_of_symbol  : G.symbol -> float

  val default_prelude     : Format.formatter -> unit
  val default_terminal    : G.terminal -> string option
  val default_nonterminal : G.nonterminal -> string option
end

module Make (G : G) : S with module G = G = struct
  module G = G
  open G

  let cost_of_attributes prj attrs =
    List.fold_left
      (fun total attr ->
         if is_attribute "cost" attr then
           total +. float_of_string (string_of_stretch (snd attr))
         else total)
      0. (prj attrs)

  let cost_of_symbol =
    let measure ~default prj attrs =
      if List.exists (is_attribute "recovery") (prj attrs) then
        cost_of_attributes prj attrs
      else default
    in
    let ft = Terminal.tabulate
        (fun t ->
           if Terminal.typ t = None
           then measure ~default:0.0 Terminal.attributes t
           else measure ~default:infinity Terminal.attributes t)
    in
    let fn =
      Nonterminal.tabulate (measure ~default:infinity Nonterminal.attributes)
    in
    function
    | T t -> ft t
    | N n -> fn n

  let cost_of_prod =
    Production.tabulate (cost_of_attributes Production.attributes)

  let penalty_of_item =
    let f = Production.tabulate @@ fun p ->
      Array.map (cost_of_attributes (fun (_,_,a) -> a))
        (Production.rhs p)
    in
    fun (p,i) ->
      let costs = f p in
      if i < Array.length costs then costs.(i) else cost_of_prod p

  let default_prelude ppf =
    List.iter (fun a ->
        if is_attribute "header" a || is_attribute "recovery.header" a then
          Format.fprintf ppf "%s\n" (string_of_stretch (snd a))
      ) Grammar.attributes

  let default_printer ?(fallback="raise Not_found") attrs =
    match List.find (is_attribute "recovery") attrs with
    | exception Not_found -> fallback
    | (_, stretch) -> string_of_stretch stretch

  let default_terminal t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
        let fallback = match Terminal.typ t with
          | None -> Some "()"
          | Some _ -> None
        in
        Some (default_printer ?fallback (Terminal.attributes t))
    | `PSEUDO -> None

  let default_nonterminal n =
    match Nonterminal.kind n with
    | `REGULAR -> Some (default_printer (Nonterminal.attributes n))
    | `START -> None
end
