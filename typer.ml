type item = Chunk.sync * Env.t
type sync = item History.sync
type t = item History.t

let initial_env =
  let initial = ref None in
  fun () ->
    match !initial with
      | Some env -> env
      | None ->
          let env = Compile.initial_env () in
          initial := Some env;
          env

let rec gather_defs acc = function
  | Chunk.Definition (d,t) -> gather_defs (d :: acc) t
  | t -> acc, t

let rec append_step chunk_item env =
  match chunk_item with
    | Chunk.Root -> env
    (* Not handled *)
    | Chunk.Module_opening (_,_,_,t) -> append_step t env
    | Chunk.Definition _ ->
        let defs, t = gather_defs [] chunk_item in
        let _,_,env = Typemod.type_toplevel_phrase env defs in
        append_step t env

let sync chunks t =
  (* Find last synchronisation point *)
  let chunks, t = History.sync fst chunks t in
  (* Drop out of sync items *)
  let t, out_of_sync = History.split t in
  (* Process last items *) 
  let last_sync,env = match History.prev t with
    | None -> History.sync_origin, initial_env ()
    | Some item -> item
  in
  let rec aux chunks t env =
    match History.forward chunks with
      | None -> t, env
      | Some ((_,chunk_item),chunks') ->
          let env = append_step chunk_item env in
          let t = History.insert (History.sync_point chunks', env) t in
          aux chunks' t env
  in
  let t, env = aux chunks t env in
  t
