type state = Env.t * (Typedtree.structure * Types.signature) list * exn list
type item = Chunk.sync * (state * state list)
type sync = item History.sync
type t = item History.t

let initial_env = Lazy.from_fun Compile.initial_env

let env t =
  match History.prev t with
    | None -> Lazy.force initial_env
    | Some (_,((env,_,_),_)) -> env

let trees t =
  match History.prev t with
    | None -> []
    | Some (_,((_,trees,_),_)) -> trees

let exns t =
  match History.prev t with
    | None -> []
    | Some (_,((_,_,exns),_)) -> exns

let rec append_step ~stop_at chunk_item env trees exns stack =
  match chunk_item with
    | Chunk.Root -> env, trees, exns, stack
    | _ when stop_at chunk_item -> env, trees, exns, stack
    | Chunk.Module_opening (_,_,pmod,t) ->
        let env, trees, exns, stack = append_step ~stop_at t env trees exns stack in
        let stack = (env, trees, exns) :: stack in
        begin try
          let open Typedtree in
          let open Parsetree in
          let rec filter_constraint md =
            let update f = function
              | None -> None
              | Some md' -> Some (f md') 
            in
            match md.pmod_desc with
              | Pmod_structure _ -> Some md
              | Pmod_functor (a,b,md) ->
                  update
                    (fun md' -> {md with pmod_desc = Pmod_functor (a,b,md')})
                    (filter_constraint md)
              | Pmod_constraint (md,_) ->
                  update (fun x -> x) (filter_constraint md)
              | _ -> None
          in
          let pmod = match filter_constraint pmod with
            | Some pmod' -> pmod'
            | None -> pmod
          in
          let rec find_structure md =
            match md.mod_desc with
              | Tmod_structure _ -> Some md
              | Tmod_functor (_,_,_,md) -> find_structure md
              | Tmod_constraint (md,_,_,_) -> Some md
              | _ -> None
          in
          let tymod = Typemod.type_module env pmod in
          match find_structure tymod with
            | None -> env, trees, exns, stack
            | Some md -> md.mod_env, trees, exns, stack
          with exn -> env, trees, (exn :: exns), stack
        end

    | Chunk.Definition (d,t) ->
        begin
          let env, trees, exns, stack = append_step ~stop_at t env trees exns stack in
          try
            let tstr,tsg,env = Typemod.type_structure env [d.Location.txt] d.Location.loc in
            env, (tstr,tsg) :: trees, exns, stack
          with exn -> env, trees, (exn :: exns), stack
        end

    | Chunk.Module_closing (d,t) ->
        begin
          let env, trees, exns, stack = append_step ~stop_at t env trees exns stack in
          let env, trees, exns, stack = match stack with
            | (env, trees, exns) :: stack -> env, trees, exns, stack
            | [] -> env, trees, exns, stack (* FIXME: should not happen, find why it happens *)
          in
          try
            let tstr,tsg,env = Typemod.type_structure env [d.Location.txt] d.Location.loc in
            env, (tstr,tsg) :: trees, exns, stack
          with exn -> env, trees, (exn :: exns), stack
        end


let sync chunks t =
  (* Find last synchronisation point *)
  let chunks, t = History.Sync.nearest fst chunks t in
  (* Drop out of sync items *)
  let t = History.cutoff t in
  (* Process last items *)
  let last_sync,((env,trees,exns),stack) = match History.prev t with
    | None -> History.Sync.origin, ((Lazy.force initial_env, [], []), [])
    | Some item -> item
  in
  let stop_at =
    match History.Sync.item last_sync with
      | None -> fun _ -> false
      | Some (_,b) -> (==) b
  in
  let rec aux chunks t env trees exns stack =
    match History.forward chunks with
      | None -> t, env, trees, exns, stack
      | Some ((_,chunk_item),chunks') ->
          (*prerr_endline "SYNC TYPER";*)
          let env, trees, exns, stack = append_step ~stop_at chunk_item env trees exns stack in 
          let t = History.insert (History.Sync.at chunks', ((env, trees, exns), stack)) t in
          aux chunks' t env trees exns stack
  in
  let t, env, trees, exns, stack = aux chunks t env trees exns stack in
  t
