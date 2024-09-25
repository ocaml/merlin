open Std

let { Logger.log } = Logger.for_section "Pipeline"

let time_shift = ref 0.0

let timed_lazy r x =
  lazy
    (let start = Misc.time_spent () in
     let time_shift0 = !time_shift in
     let update () =
       let delta = Misc.time_spent () -. start in
       let shift = !time_shift -. time_shift0 in
       time_shift := time_shift0 +. delta;
       r := !r +. delta -. shift
     in
     match Lazy.force x with
     | x ->
       update ();
       x
     | exception exn ->
       update ();
       Std.reraise exn)

module Cache = struct
  let cache = ref []

  (* Values from configuration that are used as a key for the cache.
     These values should:
     - allow to maximize reuse; associating a single typechecker instance to a
       filename and directory is natural, but keying also based on verbosity
       makes no sense
     - prevent reuse in different environments (if there is a change in
       loadpath, a new typechecker should be produced).

     It would be better to guarantee that the typechecker was well-behaved
     when the loadpath changes (so that we can reusing the same instance, and
     let the typechecker figure which part of its internal state should be
     invalidated).
     However we already had many bug related to that.  There are subtle changes
     in the type checker behavior across the different versions of OCaml.
     It is simpler to create new instances upfront.
  *)

  let key config =
    Mconfig.
      ( config.query.filename,
        config.query.directory,
        config.ocaml,
        { config.merlin with log_file = None; log_sections = [] } )

  let get config =
    let title = "pop_cache" in
    let key = key config in
    match List.assoc key !cache with
    | state ->
      cache := (key, state) :: List.remove_assoc key !cache;
      log ~title "found entry for this configuration";
      state
    | exception Not_found ->
      log ~title "nothing cached for this configuration";
      let state = Mocaml.new_state () in
      cache := (key, state) :: List.take_n 5 !cache;
      state
end

module Typer = struct
  type t = { errors : exn list lazy_t; result : Mtyper.result }
end

module Ppx = struct
  type t =
    { config : Mconfig.t; errors : exn list; parsetree : Mreader.parsetree }
end

module Reader = struct
  type t =
    { result : Mreader.result; config : Mconfig.t; cache_version : int option }
end

type t =
  { config : Mconfig.t;
    state : Mocaml.typer_state;
    raw_source : Msource.t;
    source : (Msource.t * Mreader.parsetree option) lazy_t;
    reader : Reader.t lazy_t;
    ppx : Ppx.t lazy_t;
    typer : Typer.t lazy_t;
    pp_time : float ref;
    reader_time : float ref;
    ppx_time : float ref;
    typer_time : float ref;
    error_time : float ref;
    ppx_cache_hit : bool ref;
    reader_cache_hit : bool ref;
    typer_cache_stats : Mtyper.typer_cache_stats ref
  }

let raw_source t = t.raw_source

let input_config t = t.config
let input_source t = fst (Lazy.force t.source)

let with_pipeline t f =
  Mocaml.with_state t.state @@ fun () ->
  Mreader.with_ambient_reader t.config (input_source t) f

let get_lexing_pos t pos =
  Msource.get_lexing_pos (input_source t)
    ~filename:(Mconfig.filename t.config)
    pos

let reader t = Lazy.force t.reader

let ppx t = Lazy.force t.ppx
let typer t = Lazy.force t.typer

let reader_config t = (reader t).config
let reader_parsetree t = (reader t).result.Mreader.parsetree
let reader_comments t = (reader t).result.Mreader.comments
let reader_lexer_keywords t = (reader t).result.Mreader.lexer_keywords
let reader_lexer_errors t = (reader t).result.Mreader.lexer_errors
let reader_parser_errors t = (reader t).result.Mreader.parser_errors

let reader_no_labels_for_completion t =
  (reader t).result.Mreader.no_labels_for_completion

let ppx_parsetree t = (ppx t).Ppx.parsetree
let ppx_errors t = (ppx t).Ppx.errors

let final_config t = (ppx t).Ppx.config

let typer_result t = (typer t).Typer.result
let typer_errors t = Lazy.force (typer t).Typer.errors

module Reader_phase = struct
  type t =
    { source : Msource.t * Mreader.parsetree option;
      for_completion : Msource.position option;
      config : Mconfig.t
    }

  type output = { result : Mreader.result; cache_version : int }

  let f =
    let cache_version = ref 0 in
    fun { source; for_completion; config } ->
      let result = Mreader.parse ?for_completion config source in
      incr cache_version;
      { result; cache_version = !cache_version }

  let title = "Reader phase"

  module Fingerprint = struct
    type t = Msource.Digest.t

    let make { source = source, _; _ } = Ok (Msource.Digest.make source)
    let equal = Msource.Digest.equal
  end
end

module Reader_with_cache = Phase_cache.With_cache (Reader_phase)

module Ppx_phase = struct
  type reader_cache = Off | Version of int
  type t =
    { parsetree : Mreader.parsetree;
      config : Mconfig.t;
      reader_cache : reader_cache
    }
  type output = Mreader.parsetree

  let f { parsetree; config; _ } = Mppx.rewrite parsetree config
  let title = "PPX phase"

  module Single_fingerprint = struct
    type t = { binary_id : File_id.t; args : string list; workdir : string }

    let make ~binary ~args ~workdir =
      let qualified_binary = Filename.concat workdir binary in
      match File_id.get_res qualified_binary with
      | Ok binary_id -> Ok { binary_id; args; workdir }
      | Error err -> Error err

    let equal { binary_id = b1; args = a1; workdir = w1 }
        { binary_id = b2; args = a2; workdir = w2 } =
      File_id.check b1 b2
      && List.same ~f:String.equal a1 a2
      && String.equal w1 w2
  end

  module Fingerprint = struct
    type t = Single_fingerprint.t list * reader_cache

    let make { config; reader_cache; _ } =
      let rec all_fingerprints acc = function
        | [] -> acc
        | { Std.workdir; workval } :: tl -> (
          match Std.String.split_on_char ~sep:' ' workval with
          | [] -> Error ("unhandled workval" ^ workval)
          | binary :: args ->
            Result.bind
              ~f:(fun fp ->
                all_fingerprints (Result.map ~f:(List.cons fp) acc) tl)
              (Single_fingerprint.make ~binary ~args ~workdir))
      in
      Result.map (all_fingerprints (Ok []) config.ocaml.ppx) ~f:(fun l ->
          (l, reader_cache))

    let equal_cache_version cv1 cv2 =
      match (cv1, cv2) with
      | Off, _ | _, Off -> false
      | Version v1, Version v2 -> Int.equal v1 v2

    let equal (f1, rcv1) (f2, rcv2) =
      equal_cache_version rcv1 rcv2
      && List.equal ~eq:Single_fingerprint.equal f1 f2
  end
end

module Ppx_with_cache = Phase_cache.With_cache (Ppx_phase)

let process ?state ?(pp_time = ref 0.0) ?(reader_time = ref 0.0)
    ?(ppx_time = ref 0.0) ?(typer_time = ref 0.0) ?(error_time = ref 0.0)
    ?(ppx_cache_hit = ref false) ?(reader_cache_hit = ref false)
    ?(typer_cache_stats = ref Mtyper.Miss) ?for_completion config raw_source =
  let state =
    match state with
    | None -> Cache.get config
    | Some state -> state
  in
  let source =
    timed_lazy pp_time
      (lazy
        (match Mconfig.(config.ocaml.pp) with
        | None -> (raw_source, None)
        | Some { workdir; workval } -> (
          let source = Msource.text raw_source in
          match
            Pparse.apply_pp ~workdir
              ~filename:Mconfig.(config.query.filename)
              ~source ~pp:workval
          with
          | `Source source -> (Msource.make source, None)
          | (`Interface _ | `Implementation _) as ast -> (raw_source, Some ast))))
  in
  let reader =
    timed_lazy reader_time
      (lazy
        (let (lazy ((_, pp_result) as source)) = source in
         let config = Mconfig.normalize config in
         Mocaml.setup_reader_config config;
         let cache_disabling =
           match (config.merlin.use_ppx_cache, pp_result) with
           | false, _ -> Some "configuration"
           | true, Some _ ->
             (* The cache could be refined in the future to also act on the
                PP phase. For now, let's disable the whole cache when there's
                a PP. *)
             Some "source preprocessor usage"
           | true, None -> None
         in
         let { Reader_with_cache.output = { result; cache_version };
               cache_was_hit
             } =
           Reader_with_cache.apply ~cache_disabling
             { source; for_completion; config }
         in
         reader_cache_hit := cache_was_hit;
         let cache_version =
           if Option.is_some cache_disabling then None else Some cache_version
         in
         { Reader.result; config; cache_version }))
  in
  let ppx =
    timed_lazy ppx_time
      (lazy
        (let (lazy
               { Reader.result = { Mreader.parsetree; _ };
                 config;
                 cache_version
               }) =
           reader
         in
         let caught = ref [] in
         Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught
         @@ fun () ->
         (* Currently the cache is invalidated even for source changes that don't
             change the parsetree. To avoid that, we'd have to digest the
             parsetree in the cache. *)
         let cache_disabling, reader_cache =
           match cache_version with
           | Some v -> (None, Ppx_phase.Version v)
           | None -> (Some "reader cache is disabled", Off)
         in
         let { Ppx_with_cache.output = parsetree; cache_was_hit } =
           Ppx_with_cache.apply ~cache_disabling
             { parsetree; config; reader_cache }
         in
         ppx_cache_hit := cache_was_hit;
         { Ppx.config; parsetree; errors = !caught }))
  in
  let typer =
    timed_lazy typer_time
      (lazy
        (let (lazy { Ppx.config; parsetree; _ }) = ppx in
         Mocaml.setup_typer_config config;
         let result = Mtyper.run config parsetree in
         let errors = timed_lazy error_time (lazy (Mtyper.get_errors result)) in
         typer_cache_stats := Mtyper.get_cache_stat result;
         { Typer.errors; result }))
  in
  { config;
    state;
    raw_source;
    source;
    reader;
    ppx;
    typer;
    pp_time;
    reader_time;
    ppx_time;
    typer_time;
    error_time;
    ppx_cache_hit;
    reader_cache_hit;
    typer_cache_stats
  }

let make config source = process (Mconfig.normalize config) source

let for_completion position
    { config;
      state;
      raw_source;
      pp_time;
      reader_time;
      ppx_time;
      typer_time;
      error_time;
      _
    } =
  process config raw_source ~for_completion:position ~state ~pp_time
    ~reader_time ~ppx_time ~typer_time ~error_time

let timing_information t =
  [ ("pp", !(t.pp_time));
    ("reader", !(t.reader_time));
    ("ppx", !(t.ppx_time));
    ("typer", !(t.typer_time));
    ("error", !(t.error_time))
  ]

let cache_information t =
  let typer =
    match !(t.typer_cache_stats) with
    | Miss -> `String "miss"
    | Hit { reused; typed } ->
      `Assoc [ ("reused", `Int reused); ("typed", `Int typed) ]
  in
  let fmt_hit_miss h m = `Assoc [ ("hit", `Int h); ("miss", `Int m) ] in
  let cmt_stat = Cmt_cache.get_cache_stats () in
  let cmt = fmt_hit_miss cmt_stat.hit cmt_stat.miss in
  let cmi_stat = Cmi_cache.get_cache_stats () in
  let cmi = fmt_hit_miss cmi_stat.hit cmi_stat.miss in
  Cmt_cache.clear_cache_stats ();
  Cmi_cache.clear_cache_stats ();
  let fmt_bool hit = `String (if hit then "hit" else "miss") in
  `Assoc
    [ ("reader_phase", fmt_bool !(t.reader_cache_hit));
      ("ppx_phase", fmt_bool !(t.ppx_cache_hit));
      ("typer", typer);
      ("cmt", cmt);
      ("cmi", cmi)
    ]
