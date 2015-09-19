(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Misc

(* Stateful parts:
   - lexer keywords, done Lexer
   - typer snapshot & env, done Typer
   - compiler path, done Project
   - compiler flags, done Project
*)
module Lexer  = Merlin_lexer
module Parser = Merlin_parser
module Typer  = Merlin_typer
module Recover = Merlin_recover

(* Project configuration *)
module Project : sig
  type t

  (* A global store mapping (.merlin-)path to projects *)
  val get : string list -> t * [`Fresh | `Cached]

  (* Current buffer path *)
  val set_local_path : t -> string list -> unit

  (* Project-wide configuration *)
  val check_dot_merlin : t -> unit
  val get_dot_merlins_failure : t -> (string * exn) list

  (* paths of dot_merlins *)
  val get_dot_merlins : t -> string list

  (* Dump all the flags given to merlin. *)
  val get_flags : t -> (string * string list list) list

  (* Path configuration *)
  val source_path : t -> string list
  val build_path  : t -> string list
  val cmt_path    : t -> string list

  (* List all top modules of current project *)
  val global_modules : t -> string list

  (* Enabled extensions *)
  val extensions: t -> Extension.set

  (* Lexer keywords for current config *)
  val keywords: t -> Lexer.keywords

  (* Make global state point to current project *)
  val setup : t -> unit

  (* User config override *)
  val get_user_config : t -> Dot_merlin.config
  val set_user_config : t -> Dot_merlin.config -> unit
  val get_user_config_failures : t -> (string * exn) list

  (* Invalidate cache *)
  val validity_stamp: t -> bool ref
end = struct

  module C = struct (* work around labels without disambiguation *)
    type config = {
      dot_config     : Dot_merlin.config;
      flags          : Clflags.set;
      warnings       : Warnings.state;
      keywords       : Lexer.keywords;
      extensions     : Extension.set;
      validity_stamp : bool ref;
      ppxsetup       : Ppxsetup.t;

      source_path    : string list;
      cmt_path       : string list;
      build_path     : string list;

      dot_failures  : (string * exn) list;
      user_failures : (string * exn) list;
    }
  end
  open C

  type t = {
    dot_merlin          : Dot_merlin.t;
    mutable user_config : Dot_merlin.config;
    mutable local_path  : string list;
    mutable config        : config option;
  }

  let compute_packages prj =
    let `Failures dfails, dpaths, dppxs =
      Dot_merlin.path_of_packages (Dot_merlin.config prj.dot_merlin)
    and `Failures ufails, upaths, uppxs =
      Dot_merlin.path_of_packages prj.user_config
    in
    dfails, ufails, upaths @ dpaths, Ppxsetup.union uppxs dppxs

  let compute_flags prj =
    let flags = Clflags.copy Clflags.initial in
    let warnings = Warnings.copy Warnings.initial in
    let spec = Clflags.arg_spec flags @ Warnings.arg_spec warnings in
    let process_flags spec flags =
      let failures = ref [] in
      let rec loop ?(current=(ref 0)) flags =
        try Arg.parse_argv ~current flags spec (fun flg -> raise (Arg.Bad flg)) "" with
        | Arg.Bad _ ->
          Logger.info Logger.Section.project_load ~title:"flags"
            (sprintf "unknown flag: %s" flags.(!current));
          failures := (flags.(!current), Arg.Bad flags.(!current)) :: !failures ;
          loop ~current flags
        | Arg.Help _ -> (* ignore *)
          loop ~current flags
      in
      loop flags ;
      !failures
    in
    let process_flags_list lst =
      List.fold_left lst ~init:[] ~f:(fun acc lst ->
        let flags = Array.of_list ("merlin" :: lst) in
        List.rev_append (process_flags spec flags) acc
      )
    in
    let dfails = process_flags_list (Dot_merlin.config prj.dot_merlin).Dot_merlin.flags in
    let dfails = List.rev_append (process_flags (Main_args.flags @ spec) Sys.argv) dfails in
    let ufails = process_flags_list prj.user_config.Dot_merlin.flags in
    dfails, ufails, flags, warnings

  let config prj =
    let dot_config = Dot_merlin.config prj.dot_merlin in
    match prj.config with
    | Some config when Dot_merlin.same config.dot_config dot_config -> config
    | None | Some _ ->
      begin match prj.config with
        | None -> ()
        | Some config -> config.validity_stamp := false
      end;
      let dfails0, ufails0, pkgpaths, ppxsetup = compute_packages prj in
      let dfails1, ufails1, flags, warnings = compute_flags prj in
      let open Dot_merlin in
      let user_config = prj.user_config in
      let stdlib =
          if flags.Clflags.std_include then
            [if user_config.stdlib =
                empty_config.stdlib
             then dot_config.stdlib
             else user_config.stdlib]
          else []
      in
      let source_path =
          user_config.source_path @
          dot_config.source_path @
          pkgpaths
      and build_path =
          user_config.cmi_path @
          user_config.build_path @
          dot_config.cmi_path @
          dot_config.build_path @
          pkgpaths @
          !(flags.Clflags.include_dirs) @
          stdlib
      and cmt_path =
          user_config.cmt_path @
          user_config.Dot_merlin.build_path @
          dot_config.cmt_path @
          dot_config.build_path @
          pkgpaths @
          !(flags.Clflags.include_dirs) @
          stdlib
      in
      let extensions = Extension.from
          ~extensions:(user_config.extensions @ dot_config.extensions)
          ~packages:(user_config.packages @ dot_config.packages)
      in
      let keywords = Extension.keywords extensions in
      let config = C.({
          dot_config;
          warnings; flags;
          extensions; keywords; ppxsetup;
          source_path; cmt_path; build_path;
          dot_failures = dfails0 @ dfails1;
          user_failures = ufails0 @ ufails1;
          validity_stamp = ref true;
        })
      in
      prj.config <- Some config;
      config

  let source_path p = p.local_path @ (config p).source_path
  let build_path  p = p.local_path @ (config p).build_path
  let cmt_path    p = p.local_path @ (config p).cmt_path

  let global_modules p =
    Misc.modules_in_path ~ext:".cmi" (build_path p)

  let set_local_path project path =
    project.local_path <- path

  let get_flags project = [
    "user", project.user_config.Dot_merlin.flags;
    "cmd line", [ List.tl @@ Array.to_list Sys.argv ];
    ".merlin", (Dot_merlin.config project.dot_merlin).Dot_merlin.flags;
  ]

  let get_user_config t = t.user_config
  let set_user_config t user_config =
    t.user_config <- user_config;
    t.config <- None

  let get_user_config_failures t = (config t).user_failures

  let create dot_merlins = {
    dot_merlin = Dot_merlin.load dot_merlins;
    user_config = Dot_merlin.empty_config;
    local_path = [];
    config = None;
  }

  let store : (string list, t) Hashtbl.t = Hashtbl.create 3
  let get path =
    try Hashtbl.find store path, `Cached
    with Not_found ->
      let project = create path in
      Hashtbl.replace store path project;
      project, `Fresh

  let check_dot_merlin project =
    Dot_merlin.update project.dot_merlin

  let get_dot_merlins project =
    (Dot_merlin.config project.dot_merlin).Dot_merlin.dot_merlins

  let get_dot_merlins_failure t =
    (config t).dot_failures

  (* Make global state point to current project *)
  let setup t =
    let c = config t in
    Clflags.set := c.flags;
    Warnings.current := c.warnings;
    Config.load_path := build_path t

  (* Enabled extensions *)
  let extensions t = (config t).extensions

  (* Lexer keywords for current config *)
  let keywords t = (config t).keywords

  let validity_stamp t =
    let r = (config t).validity_stamp in
    assert !r; r
end

module Buffer : sig
  type t
  val create: ?dot_merlins:string list -> ?path:string -> Parser.state -> t

  val unit_name : t -> string
  val project: t -> Project.t

  val lexer: t -> (exn list * Lexer.item) History.t
  val update: t -> (exn list * Lexer.item) History.t -> [`Nothing_done | `Updated]
  val start_lexing: ?pos:Lexing.position -> t -> Lexer.t
  val lexer_errors: t -> exn list

  val comments: t -> (string * Location.t) list

  val parser: t -> Parser.t
  val parser_errors: t -> exn list

  val recover: t -> Recover.t
  val recover_history : t -> (Lexer.item * Recover.t) History.t

  val typer: t -> Typer.t

  val get_mark: t -> Parser.frame option
  val has_mark: t -> Parser.frame option -> bool

  val is_implementation : t -> bool

  (* All top modules of current project, with current module removed *)
  val global_modules: t -> string list

  (* Try to do a background job, return false if nothing has to be done *)
  val idle_job : t -> bool
end = struct
  type t = {
    kind: Parser.state;
    path: string option;
    dot_merlins: string list;
    unit_name : string;
    mutable project : Project.t;
    mutable stamp : bool ref;
    mutable keywords: Lexer.keywords;
    mutable lexer: (exn list * Lexer.item) History.t;
    mutable recover: (Lexer.item * Recover.t) History.t;
    mutable typer: Typer.t;
  }

  let invalidate t =
    t.stamp := false;
    t.stamp <- ref true

  let is_implementation { kind ; _ } = kind = Parser.implementation

  let initial_step kind (_,token) =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Recover.fresh (Parser.from kind input))

  let autoreload_dot_merlin buffer =
    let project' = buffer.project in
    let project, status = Project.get buffer.dot_merlins in
    buffer.project <- project;
    match status with
    | `Fresh -> invalidate buffer
    | `Cached ->
      Project.check_dot_merlin project;
      if project' != project then
        invalidate buffer

  let create ?(dot_merlins=[]) ?path kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let dot_merlins = match dot_merlins, path with
      | [], Some path -> [path]
      | [], None -> []
      | xs, cwd -> List.map ~f:(Misc.canonicalize_filename ?cwd) xs
    in
    let unit_name =
      try String.sub filename ~pos:0 ~len:(String.index filename '.')
      with Not_found -> filename
    in
    let unit_name = String.capitalize unit_name in
    let lexer = Lexer.empty ~filename in
    let project =
      match Project.get dot_merlins with
      | project, `Fresh -> project
      | project, `Cached ->
        Project.check_dot_merlin project;
        project
    in
    let stamp = ref true in
    Project.setup project;
    {
      dot_merlins; path; project; lexer; kind; unit_name; stamp;
      keywords = Project.keywords project;
      recover = History.initial (initial_step kind (History.focused lexer));
      typer = Typer.fresh
          ~unit_name ~stamp:[Project.validity_stamp project; stamp]
          (Project.extensions project);
    }

  let setup buffer =
    autoreload_dot_merlin buffer;
    begin match buffer.path with
      | Some path ->
          Project.set_local_path buffer.project [path];
          begin try
            Sys.chdir path
          with _ -> ()
          end
      | None -> ()
    end;
    Project.setup buffer.project

  let unit_name t = t.unit_name

  let project t = t.project

  let lexer b = b.lexer
  let lexer_errors b = fst (History.focused b.lexer)

  let recover_history b = b.recover
  let recover b = snd (History.focused b.recover)

  let comments b = Recover.comments (recover b)

  let parser b = Recover.parser (recover b)
  let parser_errors b = Recover.exns (recover b)

  let typer b =
    setup b;
    let valid = Typer.is_valid b.typer &&
                String.Set.equal
                  (Typer.extensions b.typer)
                  (Project.extensions b.project) in
    if not valid then
        b.typer <- Typer.fresh
            ~unit_name:b.unit_name
            ~stamp:[Project.validity_stamp b.project; b.stamp]
            (Project.extensions b.project);
    b.typer <- Typer.update (parser b) b.typer;
    b.typer

  let update t l =
    t.lexer <- l;
    let strong_check (_,token) (token',_) = token == token' in
    let weak_check (_,token) (token',_) = Lexer.equal token token' in
    let init token = initial_step t.kind token in
    let strong_fold (_,token) (_,recover) = token, Recover.fold token recover in
    let weak_update (_,token) (_,recover) = (token,recover) in
    let recover', updated = History.sync t.lexer (Some t.recover)
        ~init ~strong_check ~strong_fold ~weak_check ~weak_update; in
    t.recover <- recover';
    updated

  let start_lexing ?pos b =
    let kw = Project.keywords b.project in
    if kw != b.keywords then begin
      b.keywords <- kw;
      ignore (update b (History.drop_tail (History.seek_backward
                                             (fun _ -> true) b.lexer)))
    end
    else begin
      let item_pred pos_pred = function
        | _, Lexer.Valid (cur,_,_) when pos_pred cur -> true
        | _, Lexer.Valid (p,_,_) when p = Lexing.dummy_pos -> true
        | _, Lexer.Error _ -> true
        | _ -> false
      in
      let lexer = b.lexer in
      let lexer = match pos with
        | None -> lexer
        | Some pos ->
          let line, _ = Lexing.split_pos pos in
          let pos_pred cur =
            let line', _ = Lexing.split_pos cur in
            line > line'
          in
          History.seek_forward (item_pred pos_pred) lexer
      in
      let pos_pred = match pos with
        | None -> (fun _ -> false)
        | Some pos ->
          let line, _ = Lexing.split_pos pos in
          (fun cur -> let line', _ = Lexing.split_pos cur in line <= line')
      in
      let lexer = History.seek_backward (item_pred pos_pred) lexer in
      let lexer = History.move (-1) lexer in
      ignore (update b lexer)
    end;
    Lexer.start kw b.lexer

  let get_mark t = Parser.find_marker (parser t)

  let has_mark t = function
    | None -> false
    | Some frame -> Parser.has_marker (parser t) frame

  let global_modules t =
    setup t;
    List.remove t.unit_name (Project.global_modules t.project)

  exception Break
  let idle_job t =
    Typer.with_typer (typer t) @@ fun () ->
    Clflags.real_paths () <> `Real &&
    let concr = Env.used_persistent () in
    Types.Concr.exists Printtyp.compute_map_for_pers concr

end
