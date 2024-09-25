open Std

module Path : sig
  val is_opened : Env.t -> Path.t -> bool

  val to_shortest_lid :
    env:Env.t ->
    ?name:string ->
    env_check:(Longident.t -> Env.t -> 'a) ->
    Path.t ->
    Longident.t
end = struct
  let opens env =
    let rec aux acc = function
      | Env.Env_open (s, path) -> aux (path :: acc) s
      | s ->
        Option.map ~f:(aux acc) (Browse_misc.summary_prev s)
        |> Option.value ~default:acc
    in
    aux [] env

  let is_opened env path = List.mem path ~set:(opens (Env.summary env))

  let rec to_shortest_lid ~(opens : Path.t list) = function
    | Path.Pdot (path, name) when List.exists ~f:(Path.same path) opens ->
      Longident.Lident name
    | Path.Pdot (path, name) -> Ldot (to_shortest_lid ~opens path, name)
    | Pident ident -> Lident (Ident.name ident)
    | _ -> assert false

  let maybe_replace_name ?name lid =
    let open Longident in
    Option.value_map name ~default:lid ~f:(fun name ->
        match lid with
        | Lident _ -> Lident name
        | Ldot (lid, _) -> Ldot (lid, name)
        | _ -> assert false)

  let to_shortest_lid ~env ?name ~env_check path =
    let opens = opens (Env.summary env) in
    let lid = to_shortest_lid ~opens path |> maybe_replace_name ?name in
    try
      env_check lid env |> ignore;
      lid
    with Not_found -> maybe_replace_name ?name (Untypeast.lident_of_path path)
end

let parenthesize_name name =
  (* Qualified operators need parentheses *)
  if name = "" || not (Oprint.parenthesized_ident name) then name
  else if name.[0] = '*' || name.[String.length name - 1] = '*' then
    "( " ^ name ^ " )"
  else "(" ^ name ^ ")"

let parse_identifier (config, source) pos =
  let path = Mreader.reconstruct_identifier config source pos in
  let path = Mreader_lexer.identifier_suffix path in
  Logger.log ~section:Type_enclosing.log_section ~title:"reconstruct-identifier"
    "paths: [%s]"
    (String.concat ~sep:";" (List.map path ~f:(fun l -> l.Location.txt)));
  path
