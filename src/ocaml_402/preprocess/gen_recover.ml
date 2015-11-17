open MenhirSdk
open Cmly_format

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly"
    Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-v" then
      verbose := true
    else if !name = "" then
      name := Sys.argv.(i)
    else
      usage ()
  done;
  if !name = "" then
    usage ()

let g = Cmly_io.read_file !name

(** Misc routines *)

let is_attribute name (name', stretch : attribute) =
  name = Positions.value name'

let string_of_stretch s =
  s.Stretch.stretch_raw_content

let string_of_type = function
  | Stretch.Inferred s -> s
  | Stretch.Declared s -> string_of_stretch s

let name_of_symbol = function
  | T t -> t.t_name
  | N n -> n.n_name

let report fmt =
  if !verbose then
    Printf.eprintf fmt
  else
    Printf.ifprintf stderr fmt

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

let align_tabular rows =
  let rec lengths l acc = function
    | [] when l = -1 -> []
    | [] ->
      l :: lengths (-1) [] acc
    | [] :: rows ->
      lengths l acc rows
    | (col :: cols) :: rows ->
      lengths (max l (String.length col)) (cols :: acc) rows
  in
  let lengths = lengths (-1) [] rows in
  let rec adjust_length lengths cols = match lengths, cols with
    | l :: ls, c :: cs ->
      let pad = l - String.length c in
      let c =
        if pad = 0 then c
        else c ^ String.make pad ' '
      in
      c :: adjust_length ls cs
    | _, [] -> []
    | [], _ -> assert false
  in
  List.map (adjust_length lengths) rows

module StateMap = Map.Make (struct
    type t = lr0_state
    let compare t1 t2 =
      compare t1.lr0_index t2.lr0_index
  end)

module SymbolMap = Map.Make (struct
    type t = symbol
    let compare s1 s2 = match s1, s2 with
      | T _, N _ -> -1
      | N _, T _ -> 1
      | T t1, T t2 -> compare t1.t_index t2.t_index
      | N n1, N n2 -> compare n1.n_index n2.n_index
  end)

(* Tabulation over grammatical entities *)

let table_terminal f =
  let table = Array.map f g.g_terminals in
  function t -> table.(t.t_index)

let table_nonterminal f =
  let table = Array.map f g.g_nonterminals in
  function n -> table.(n.n_index)

let table_production f =
  let table = Array.map f g.g_productions in
  function p -> table.(p.p_index)

let table_symbol f =
  let ft = table_terminal (fun t -> f (T t)) in
  let fn = table_nonterminal (fun n -> f (N n)) in
  function
  | T t -> ft t
  | N n -> fn n

let table_lr0 f =
  let tbl = Array.map f g.g_lr0_states in
  fun lr0 -> tbl.(lr0.lr0_index)

(** Cost analysis *)

let cost_of_attributes attrs =
  List.fold_left
    (fun total attr ->
       if is_attribute "cost" attr then
         total +. float_of_string (string_of_stretch (snd attr))
       else total)
    0. attrs

let null_reducible =
  let table = Array.make (Array.length g.g_nonterminals) None in
  Array.iter (fun p ->
      if p.p_kind = `REGULAR && p.p_rhs = [||] then
        table.(p.p_lhs.n_index) <- Some p
    ) g.g_productions;
  function n -> table.(n.n_index)

let cost_of_symbol =
  let measure ~default attrs =
    if List.exists (is_attribute "recovery") attrs then
      cost_of_attributes attrs
    else default
  in
  table_symbol (function
      | T t when t.t_type = None ->
        measure ~default:0.0 t.t_attributes
      | T t ->
        measure ~default:infinity t.t_attributes
      | N n when null_reducible n <> None ->
        measure ~default:0.0 n.n_attributes
      | N n ->
        measure ~default:infinity n.n_attributes
    )

let cost_of_rhs =
  let measure p =
    let costs = Array.map
        (fun (sym,_,attrs) ->
           1.0 +. cost_of_symbol sym +. cost_of_attributes attrs)
        p.p_rhs
    in
    let total = ref 0.0 in
    for i = Array.length costs - 1 downto 0 do
      total := !total +. costs.(i);
      costs.(i) <- !total
    done;
    costs
  in
  table_production measure

let cost_of_first_item =
  Array.make (Array.length g.g_productions) infinity

let cost_of_first_items, minimize_cost_of_first_item =
  let table = Hashtbl.create 7 in
  (fun predecessor prod ->
    let k = (predecessor.lr0_index, prod.p_index) in
    try Hashtbl.find table k
    with Not_found ->
      let r = ref infinity in
      Hashtbl.add table k r;
      r),
  (fun () ->
     Hashtbl.iter (fun (_,index) r ->
         cost_of_first_item.(index) <-
           min cost_of_first_item.(index) !r
       ) table)

let cost_of_raw_item (p,pos) =
  (* An item at position 0 has no incoming symbol.
     It is only possible for initial states, and those should never
     be observed by recovery engine *)
  assert (pos > 0);
  if p.p_kind = `START then infinity
  else
    let tbl = cost_of_rhs p in
    if pos = Array.length tbl then 0.
    else tbl.(pos)

let cost_of_item ?predecessor (p,pos) =
  let first =
    if pos = 1 then
      match predecessor with
      | Some predecessor -> !(cost_of_first_items predecessor p)
      | None -> cost_of_first_item.(p.p_index)
    else 0.0
  in
  first +. cost_of_raw_item (p,pos)

let items_table items =
  let last_lhs = ref (-1) in
  let prepare (p,pos) =
    let rhs = Array.map (fun (sym, id, _) ->
        if id <> "" && id.[0] <> '_' then
          "(" ^ id ^ " = " ^ name_of_symbol sym ^ ")"
        else name_of_symbol sym)
        p.p_rhs
    and costs = Array.map string_of_float (cost_of_rhs p) in
    if pos >= 0 && pos < Array.length rhs then
      rhs.(pos) <- ". " ^ rhs.(pos)
    else if pos = Array.length rhs then
      rhs.(pos - 1) <- rhs.(pos - 1) ^ " .";
    let rhs = Array.to_list rhs and costs = Array.to_list costs in
    let rhs =
      if !last_lhs = p.p_lhs.n_index then
        "" :: "  |" :: rhs
      else
        (last_lhs := p.p_lhs.n_index;
         p.p_lhs.n_name :: "::=" :: rhs)
    and costs =
      "" :: "" :: costs
    in
    [rhs; costs]
  in
  align_tabular (List.concat (List.map prepare items))

let report_table ?(prefix="") ?(sep=" ") table =
  List.iter (fun line -> report "%s%s\n" prefix (String.concat sep line))
    table

(** State analysis *)

let lr0_predecessors, lr1_predecessors =
  let tbl1 = Array.make (Array.length g.g_lr1_states) [] in
  let revert_transition s1 (sym,s2) =
    assert (match s2.lr1_lr0.lr0_incoming with
        | None -> false
        | Some sym' -> sym = sym');
    tbl1.(s2.lr1_index) <- s1 :: tbl1.(s2.lr1_index)
  in
  Array.iter
    (fun lr1 -> Array.iter (revert_transition lr1) lr1.lr1_transitions)
    g.g_lr1_states;
  let tbl0 = Array.make (Array.length g.g_lr0_states) [] in
  Array.iter (fun lr1 ->
      tbl0.(lr1.lr1_lr0.lr0_index) <-
        List.map (fun lr1 -> lr1.lr1_lr0) tbl1.(lr1.lr1_index) @
        tbl0.(lr1.lr1_lr0.lr0_index))
    g.g_lr1_states;
  (fun lr0 -> tbl0.(lr0.lr0_index)),
  (fun lr1 -> tbl1.(lr1.lr1_index))

let lr0_successors =
  let tbl0 = Array.make (Array.length g.g_lr0_states) SymbolMap.empty in
  let core_transition s1 (sym,s2) =
    tbl0.(s1.lr1_lr0.lr0_index) <-
      SymbolMap.add sym s2.lr1_lr0
        tbl0.(s1.lr1_lr0.lr0_index)
  in
  Array.iter
    (fun lr1 -> Array.iter (core_transition lr1) lr1.lr1_transitions)
    g.g_lr1_states;
  (fun lr0 -> tbl0.(lr0.lr0_index))


let classify_state =
  table_lr0 (fun lr0 ->
      let max_pos = Array.fold_left max 0 (Array.map snd lr0.lr0_items) in
      match max_pos with
      | 0 -> `Starting
      | 1 -> begin match lr0.lr0_items with
          | [| {p_rhs = [||]; p_lhs}, 1 |] ->
            assert (p_lhs.n_kind = `START);
            `Final
          | _ -> `Regular
        end
      | _ -> `Regular
    )

let starting_states, final_states, regular_states =
  let starting_states = ref [] in
  let final_states = ref [] in
  let regular_states = ref [] in
  let store_state lr0 =
    let list = match classify_state lr0 with
      | `Starting -> starting_states
      | `Final    -> final_states
      | `Regular  -> regular_states
    in
    assert ((list == starting_states) = (lr0.lr0_incoming = None));
    list := lr0 :: !list
  in
  Array.iter store_state g.g_lr0_states;
  List.rev !starting_states,
  List.rev !final_states,
  List.rev !regular_states

let () =
  (* Compute minimal cost per predecessor *)
  let transition_cost predecessor successor =
    let costs = Array.map (cost_of_item ~predecessor) successor.lr0_items in
    Array.fold_left min infinity costs
  in
  let minimize_firsts predecessor successor minimized =
    let transition_cost n =
      try
        transition_cost predecessor (SymbolMap.find (N n)
                                       (lr0_successors predecessor))
      with Not_found ->
        report "PREDECESSOR:\n";
        report_table (items_table (Array.to_list predecessor.lr0_items));
        report "STATE:\n";
        report_table (items_table (Array.to_list successor.lr0_items));
        report "SYMBOL: %S\n" n.n_name;
        raise Not_found
    in
    let minimize_item minimized (prod,p) =
      if p = 1 then
        let cost = cost_of_first_items predecessor prod in
        let cost' = transition_cost prod.p_lhs in
        if cost' < !cost then
          (cost := cost'; true)
        else
          minimized
      else minimized
    in
    Array.fold_left minimize_item minimized successor.lr0_items
  in
  let minimize minimized predecessor =
    if predecessor.lr0_incoming = None then
      minimized
    else
      SymbolMap.fold
        (fun _ successor minimized ->
           minimize_firsts predecessor successor minimized)
        (lr0_successors predecessor) minimized
  in
  let rec fix () =
    if Array.fold_left minimize false g.g_lr0_states then
      fix ()
  in
  fix ();
  minimize_cost_of_first_item ()

let regular_states =
  let annot_list f l = List.map (fun x -> f x, x) l in
  let cost_of_lr0 lr0 =
    Array.fold_left min infinity (Array.map cost_of_item lr0.lr0_items)
  in
  annot_list cost_of_lr0 regular_states

(** Reporting *)

let report_productions () =
  report "# Summary of production recovery cost\n\n";
  let last_lhs = ref (-1) in
  let items = ref [] in
  let flush_items () =
    report_table (items_table (List.rev !items));
    items := [];
  in
  let add_prod p =
    if !last_lhs <> p.p_lhs.n_index then flush_items ();
    items := (p, -1) :: !items
  in
  Array.iter (fun p -> if p.p_kind = `REGULAR then add_prod p) g.g_productions;
  flush_items ()


let report_states () =
  report "# Summary of (LR(0)) state recoveries\n\n";

  let state_index lr0 = string_of_int lr0.lr0_index in
  report "start states (no recovery) = %s\n"
    (String.concat ", " (List.rev_map state_index starting_states));
  report "final states (no recovery) = %s\n\n"
    (String.concat ", " (List.rev_map state_index final_states));

  let unrecovered_states, regular_states =
    let rec aux acc = function
      | x :: xs when fst x = infinity -> aux (x :: acc) xs
      | xs -> acc, xs
    in
    aux [] regular_states
  in

  let report_state (cost, lr0) =
    report "### cost(#%d) = %.02f\n" lr0.lr0_index cost;
    let items = Array.to_list lr0.lr0_items in
    let looping, regular = List.partition (fun (_,pos) -> pos = 1) items in
    if regular = [] then
      report_table ~prefix:"  " (items_table looping)
    else (
      report_table ~prefix:"  " (items_table regular);
      if looping <> [] then
        (report "\n; (ignored) looping items:\n";
         report_table ~prefix:"; " (items_table looping))
    );
    report "\n";
  in

  if regular_states <> [] then
    begin
      report "## states with potentially looping recovery\n\n";
      List.iter report_state regular_states;
    end;

  if unrecovered_states <> [] then
    begin
      let verbose' = !verbose in
      verbose := true;
      report "## states without recovery strategy\n\n";
      List.iter report_state unrecovered_states;
      verbose := verbose'
    end;

  if regular_states <> [] then
    begin
      report "## most expensive regular states\n\n";
      let n = List.length regular_states in
      let cost, _ = List.nth regular_states (n / 10) in
      let regular_states = List.filter (fun (c,_) -> c >= cost) regular_states in
      List.iter report_state regular_states
    end

type reduction = (production * int)
type decision = [
  | `Impossible
  | `Reduction of reduction
  | `Look_at_predecessor of
      (lr0_state * [`Reduction of reduction | `Impossible]) list
]

let decision lr0 =
  let order ?predecessor a b =
    compare (cost_of_item ?predecessor a) (cost_of_item ?predecessor b) in
  let order_items ?predecessor items = List.sort (order ?predecessor) items in
  match classify_state lr0 with
  | `Regular ->
    let items = order_items (Array.to_list lr0.lr0_items) in
    let ((prod, pos) as item) = List.hd items in
    if pos <> 1 then `Reduction item else
      let selections =
        List.map (fun predecessor ->
            let items' = order_items ~predecessor items in
            (*Printf.eprintf "state:%d pred:%d items:%d/%d items':%d\n"
              lr0.lr0_index predecessor.lr0_index
              (List.length items)
              (Array.length lr0.lr0_items)
              (List.length items');*)
            let item =
              match items' with
              | [] -> `Impossible
              | item :: items -> `Reduction item
            in
            predecessor, item
          ) (lr0_predecessors lr0)
      in
      if List.for_all ((=) (List.hd selections)) selections then
        snd (List.hd selections)
      else
        `Look_at_predecessor selections
  | `Starting | `Final -> assert false

let report_final_decision () =
  let check_cost c =
    if c = infinity then
      " (MISSING VALUES, CANNOT RECOVER)"
    else ""
  in
  let is_wrong = function
    | _, `Impossible -> true
    | _, `Reduction item -> cost_of_item item = infinity
  in
  let is_wrong = function
    | `Impossible -> true
    | `Reduction item -> cost_of_item item = infinity
    | `Look_at_predecessor x -> List.exists is_wrong x
  in
  let report_decision state decision =
    let verbose' = !verbose in
    if is_wrong decision then
      verbose := true;
    begin match decision with
    | `Impossible ->
      report "state %d: no terminating sequence found (empty language or bug?!)\n"
        state.lr0_index
    | `Reduction item ->
      let cost = cost_of_item item in
      report "state %d, at cost %.02f reduce:%s\n"
        state.lr0_index cost (check_cost cost);
      report_table ~prefix:"  " (items_table [item])
    | `Look_at_predecessor predecessors ->
      report "state %d, looking at:\n" state.lr0_index;
      List.iter (function
          | (predecessor, `Reduction item) ->
            let cost = cost_of_item item in
            report "- predecessor %d, at cost %.02f reduce:%s\n"
              predecessor.lr0_index cost (check_cost cost);
            report_table ~prefix:"    " (items_table [item]);
            verbose := verbose'
          | (predecessor, `Impossible) ->
            report "- predecessor %d, no terminating sequence found (empty language or bug?!)\n"
              predecessor.lr0_index
        ) predecessors
    end;
    verbose := verbose'
  in
  report "# Final decision\n\n";
  Array.iter (fun lr0 ->
      match classify_state lr0 with
      | `Regular -> report_decision lr0 (decision lr0)
      | `Starting | `Final -> ()
    ) g.g_lr0_states

let report () =
  report_productions ();
  report "\n";
  report_states ();
  report "\n";
  report_final_decision ();
  report "\n"

(** Printing *)

(** Print header, if any *)

let print_header () =
  let name = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
  printf "open %s\n" (String.capitalize name);
  List.iter
    (fun (_, stretch) -> printf "%s\n" (string_of_stretch stretch))
    (List.filter
       (fun a ->
         is_attribute "header" a ||
         is_attribute "recovery.header" a
       ) g.g_attributes)

(** Default value for a symbol *)

let default_printer ?(fallback="raise Not_found") attrs =
  match List.find (is_attribute "recovery") attrs with
  | exception Not_found -> fallback
  | (_, stretch) -> string_of_stretch stretch

let print_default_value () =
  let case_t t =
    match t.t_kind with
    | `REGULAR | `ERROR ->
      let fallback = match t.t_type with
        | None -> Some "()"
        | Some _ -> None
      in
      printf "  | %s.T %s.T_%s -> %s\n"
        menhir menhir t.t_name
        (default_printer ?fallback t.t_attributes)
    | `PSEUDO | `EOF -> ()
  and case_n n =
    if n.n_kind = `REGULAR then
      printf "  | %s.N %s.N_%s -> %s\n"
        menhir menhir n.n_name
        (default_printer n.n_attributes)
  in
  printf "let default_value (type a) : a %s.symbol -> a = function\n" menhir;
  Array.iter case_t g.g_terminals;
  Array.iter case_n g.g_nonterminals

let group_assoc l =
  let compare_snd (_,a) (_,b) = compare a b in
  let rec aux ks v' acc = function
    | [] -> (ks, v') :: acc
    | (k,v) :: xs ->
      if compare v v' = 0 then
        aux (k :: ks) v' acc xs
      else
        aux [k] v ((ks, v') :: acc) xs
  in
  match List.sort compare_snd l with
  | [] -> []
  | (k,v) :: xs -> aux [k] v [] xs

let rec filter_map_assoc f = function
  | (k, v) :: xs ->
    begin match f v with
      | None -> filter_map_assoc f xs
      | Some v' -> (k, v') :: filter_map_assoc f xs
    end
  | [] -> []

let print_decisions () =
  printf "type decision =\n\
         \  | Shift   : 'a %s.symbol * 'a -> decision\n\
         \  | Reduce  : int -> decision\n\
         \  | Parent  : (int -> decision) -> decision\n\n"
    menhir;
  let string_of_states ks =
    String.concat " | "
      (List.map (fun lr0 -> string_of_int lr0.lr0_index) ks)
  in
  let string_of_reduction (prod,pos) =
    if pos = Array.length prod.p_rhs then
      sprintf "Reduce %d" prod.p_index
    else
      let symbol = match prod.p_rhs.(pos) with
        | (T t, _, _) -> sprintf "T T_%s" t.t_name
        | (N n, _, _) -> sprintf "N N_%s" n.n_name
      in
      sprintf "Shift (%s, default_value (%s))" symbol symbol
  in
  let string_of_decision lr0 =
    match classify_state lr0 with
    | `Starting | `Final   -> None
    | `Regular ->
      match decision lr0 with
      | `Impossible -> None
      | `Reduction red -> Some (string_of_reduction red)
      | `Look_at_predecessor cases ->
        let cases =
          cases |>
          filter_map_assoc (function
            | `Reduction red ->
              Some (string_of_reduction red)
            | `Impossible -> None
          ) |>
          group_assoc |>
          List.map (fun (ks,v) ->
              sprintf "     | %s -> %s" (string_of_states ks) v)
        in
        Some (String.concat "\n" (
          ["Parent (function"] @
          cases @
          ["     | _ -> raise Not_found)"]
        ))
  in
  printf "let decision =\n\
         \  let open %s in function\n" menhir;
  let cases =
    g.g_lr0_states |>
    Array.to_list |>
    List.map (fun x -> x,x) |>
    filter_map_assoc string_of_decision |>
    group_assoc
  in
  List.iter (fun (states,action) ->
      printf "  | %s -> %s\n"
        (string_of_states states)
        action
    ) cases;
  printf "  | _ -> raise Not_found\n"

let print () =
  print_header ();
  print_newline ();
  print_default_value ();
  print_newline ();
  print_decisions ()

let () =
  report ();
  print ()
