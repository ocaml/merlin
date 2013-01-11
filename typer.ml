type item = Chunk.sync * Env.t * exn list
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

let rec append_step chunk_item env exns =
  match chunk_item with
    | Chunk.Root -> env, exns
    (* Not handled *)
    | Chunk.Module_opening (_,_,_,t) -> append_step t env exns
    | Chunk.Definition (d,t) ->
        let env, exns = append_step t env exns in
        try
          let _,_,env = Typemod.type_toplevel_phrase env [d] in
          env, exns
        with exn -> env, (exn :: exns)

let sync chunks t =
  (* Find last synchronisation point *)
  let chunks, t = History.sync (fun (a,_,_) -> a) chunks t in
  (* Drop out of sync items *)
  let t, out_of_sync = History.split t in
  (* Process last items *)
  let last_sync,env,exns = match History.prev t with
    | None -> History.sync_origin, initial_env (), []
    | Some item -> item
  in
  let rec aux chunks t env exns =
    match History.forward chunks with
      | None -> t, env, exns
      | Some ((_,chunk_item),chunks') ->
          let env, exns = append_step chunk_item env exns in
          let t = History.insert (History.sync_point chunks', env, exns) t in
          aux chunks' t env exns
  in
  let t, env, exns = aux chunks t env exns in
  t
