let {Logger. log} = Logger.for_section "ocamlmerlin-lsp"

let initializeInfo: Lsp.Protocol.Initialize.result = {
  server_capabilities = {
    textDocumentSync = {
      Lsp.Protocol.Initialize.
      openClose = true;
      change = IncrementalSync;
      willSave = false;
      willSaveWaitUntil = false;
      didSave = None;
    };
    hoverProvider = true;
    definitionProvider = true;
    typeDefinitionProvider = true;
    completionProvider = Some {
      Lsp.Protocol.Initialize.
      resolveProvider = false;
      triggerCharacters = ["."];
    };
    referencesProvider = false;
    documentHighlightProvider = false;
    documentSymbolProvider = true;
    workspaceSymbolProvider = false;
    codeActionProvider = false;
    codeLensProvider = None;
    documentFormattingProvider = false;
    documentRangeFormattingProvider = false;
    documentOnTypeFormattingProvider = None;
    renameProvider = false;
    documentLinkProvider = None;
    executeCommandProvider = None;
    typeCoverageProvider = false;
    rageProvider = false;
  };
}

module Document : sig
  type t

  val make :
    ?version:int
    -> uri:Lsp.Protocol.documentUri
    -> text:string
    -> unit
    -> t

  val uri : t -> Lsp.Protocol.documentUri
  val source : t -> Msource.t
  val pipeline : t -> Mpipeline.t

  val update_text : ?version:int -> Lsp.Protocol.DidChange.textDocumentContentChangeEvent -> t -> t
end = struct
  type t = {
    tdoc : Lsp.Text_document.t;
    source : Msource.t Lazy.t;
    pipeline : Mpipeline.t Lazy.t;
    config : Mconfig.t Lazy.t;
  }

  let normalize_line_endings text =
    let text = Std.String.replace_all ~pattern:"\r\n" ~with_:"\n" text in
    text

  let uri doc = Lsp.Text_document.documentUri doc.tdoc
  let source doc = Lazy.force doc.source
  let pipeline doc = Lazy.force doc.pipeline

  let make_config path =
    let config = Mconfig.initial in
    let query = config.query in
    let path = Misc.canonicalize_filename path in
    let filename = Filename.basename path in
    let directory = Filename.dirname path in
    let t = {
      config with query = {
        query with
        verbosity = 1;
        filename;
        directory;
      }
    } in
    Mconfig.load_dotmerlins t ~filenames:[
      let base = "." ^ filename ^ ".merlin" in
      Filename.concat directory base
    ]

  let make ?(version=0) ~uri ~text () =
    let tdoc = Lsp.Text_document.make ~version uri text in
    let path = Lsp.Uri.to_path uri in
    (* we can do that b/c all text positions in LSP are line/col *)
    let text = normalize_line_endings text in
    let source = lazy (Msource.make text) in
    let config = lazy (make_config path) in
    let pipeline = lazy (
      let config = Lazy.force config in
      let source = Lazy.force source in
      Mpipeline.make config source)
    in
    {tdoc; source; config; pipeline;}

  let update_text ?version change doc =
    let tdoc = Lsp.Text_document.apply_content_change ?version change doc.tdoc in
    let text = Lsp.Text_document.text tdoc in
    log ~title:"debug" "TEXT\n%s" text;
    let source = lazy (Msource.make text) in
    let pipeline = lazy (
      let config = Lazy.force doc.config in
      let source = Lazy.force source in
      Mpipeline.make config source)
    in
    {
      tdoc;
      config = doc.config;
      source; pipeline;
    }
end

module Document_store : sig
  type t
  val make : unit -> t
  val put : t -> Document.t -> unit
  val get : t -> Lsp.Protocol.documentUri -> (Document.t, string) result
  val get_opt : t -> Lsp.Protocol.documentUri -> Document.t option
end = struct
  type t = (Lsp.Protocol.documentUri, Document.t) Hashtbl.t

  let make () = Hashtbl.create 50
  let put store doc = Hashtbl.replace store (Document.uri doc) doc
  let get_opt store uri =
    try Some (Hashtbl.find store uri)
    with Not_found -> None
  let get store uri =
    match get_opt store uri with
    | Some doc -> Ok doc
    | None -> Lsp.Utils.Result.errorf "no document found with uri: %a" Lsp.Uri.pp uri
end

let logical_of_position (position : Lsp.Protocol.position) =
  let line = position.line + 1 in
  let col = position.character + 1 in
  `Logical (line, col)

let position_of_lexical_position (lex_position : Lexing.position) =
  let line = lex_position.pos_lnum - 1 in
  let character = lex_position.pos_cnum - lex_position.pos_bol in
  Lsp.Protocol.{line; character;}

let send_diagnostics rpc doc =
  let command = Query_protocol.Errors in
  let errors = Query_commands.dispatch (Document.pipeline doc) command in
  let diagnostics =
    List.map (fun (error : Location.error) ->
      let range = {
        Lsp.Protocol.
        start_ = position_of_lexical_position error.loc.loc_start;
        end_ = position_of_lexical_position error.loc.loc_end;
      } in
      let severity =
        match error.source with
        | Warning -> Some Lsp.Protocol.PublishDiagnostics.Warning
        | _ -> Some Lsp.Protocol.PublishDiagnostics.Error
      in
      let diagnostic: Lsp.Protocol.PublishDiagnostics.diagnostic = {
        Lsp.Protocol.PublishDiagnostics.
        message = error.Location.msg;
        severity;
        range;
        relatedInformation = [];
        relatedLocations = [];
        code = NoCode;
        source = None;
      } in
      diagnostic
    ) errors
  in

  let notif =
    Lsp.Rpc.Server_notification.PublishDiagnostics {
      uri = Document.uri doc;
      diagnostics = diagnostics;
    }
  in

  Lsp.Rpc.send_notification rpc notif

let on_initialize _rpc state _params =
  Ok (state, initializeInfo)

let on_request :
  type resp .
  Lsp.Rpc.t
  -> Document_store.t
  -> Lsp.Protocol.Initialize.client_capabilities
  -> resp Lsp.Rpc.Request.t
  -> (Document_store.t * resp, string) result
  = fun _rpc store client_capabilities req ->

  let open Lsp.Utils.Result.Infix in

  match req with

  | Lsp.Rpc.Request.Shutdown ->
    return (store, ())

  | Lsp.Rpc.Request.DebugTextDocumentGet {textDocument = {uri;}; position = _;} ->
    begin match Document_store.get_opt store uri with
    | None -> return (store, None)
    | Some doc -> return (store, Some (Msource.text (Document.source doc)))
    end

  | Lsp.Rpc.Request.DebugEcho params ->
    return (store, params)

  | Lsp.Rpc.Request.TextDocumentHover {textDocument = {uri;}; position;} ->
    Document_store.get store uri >>= fun doc ->
    let command =
      Query_protocol.Type_enclosing (
        None,
        logical_of_position position,
        None
      )
    in
    begin match Query_commands.dispatch (Document.pipeline doc) command with
    | [] -> return (store, None)
    | (_loc, `Index _, _is_tail) :: _rest -> return (store, None)
    | (_loc, `String contents, _is_tail) :: _rest ->
      let contents =
        if
          List.mem
            Lsp.Protocol.MarkupKind.Markdown
            client_capabilities.textDocument.hover.contentFormat
        then "```\n" ^ contents ^ "\n```"
        else contents
      in
      let resp = {
        Lsp.Protocol.Hover.
        contents = [MarkedString contents];
        range = None;
      } in
      return (store, Some resp)
    end

  | Lsp.Rpc.Request.DocumentSymbol {textDocument = {uri;}} ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = Query_commands.dispatch (Document.pipeline doc) command in
    let module SymbolInformation = Lsp.Protocol.SymbolInformation in
    let symbol_infos =
      let rec symbol_info_of_outline_item ?containerName item =
        let kind =
          match item.Query_protocol.outline_kind with
          | `Value -> SymbolInformation.Function
          | `Constructor -> SymbolInformation.Constructor
          | `Label -> SymbolInformation.Property
          | `Module -> SymbolInformation.Module
          | `Modtype -> SymbolInformation.Module
          | `Type -> SymbolInformation.String
          | `Exn -> SymbolInformation.Constructor
          | `Class -> SymbolInformation.Class
          | `Method -> SymbolInformation.Method
        in
        let location = {
          Lsp.Protocol.Location.
          uri;
          range = {
            Lsp.Protocol.
            start_ = position_of_lexical_position item.location.loc_start;
            end_ = position_of_lexical_position item.location.loc_end;
          }
        } in
        let info = {
          Lsp.Protocol.SymbolInformation.
          name = item.Query_protocol.outline_name;
          kind;
          deprecated = false;
          location;
          containerName;
        } in
        let children =
          Std.List.concat_map
            item.children
            ~f:(symbol_info_of_outline_item ~containerName:info.name)
        in
        info::children
      in
      Std.List.concat_map ~f:symbol_info_of_outline_item outline
    in
    return (store, symbol_infos)

  | Lsp.Rpc.Request.TextDocumentDefinition {textDocument = {uri;}; position;} ->
    Document_store.get store uri >>= fun doc ->
    let position = logical_of_position position in
    let command = Query_protocol.Locate (None, `ML, position) in
    begin match Query_commands.dispatch (Document.pipeline doc) command with
    | `Found (path, lex_position) ->
      let position = position_of_lexical_position lex_position in
      let range = {Lsp.Protocol. start_ = position; end_ = position;} in
      let uri =
        match path with
        | None -> uri
        | Some path -> Lsp.Uri.of_path path
      in
      let loc = {Lsp.Protocol.DefinitionLocation. uri; range; title = None;} in
      return (store, [loc])
    | `At_origin
    | `Builtin _
    | `File_not_found _
    | `Invalid_context
    | `Not_found _
    | `Not_in_env _ -> Ok (store, [])
    end

  | Lsp.Rpc.Request.TextDocumentTypeDefinition {textDocument = {uri;}; position;} ->
    Document_store.get store uri >>= fun doc ->
    let position = logical_of_position position in
    let pipeline = Document.pipeline doc in
    let typer = Mpipeline.typer_result pipeline in
    Mtyper.with_typer typer @@ fun () ->
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline position in
    let path = Mbrowse.enclosing pos [structures] in
    let path =
      let rec resolve_tlink env ty =
        match ty.Types.desc with
        | Tconstr (path, _, _) -> Some (env, path)
        | Tlink ty -> resolve_tlink env ty
        | _ -> None
      in
      Std.List.filter_map path ~f:(fun (env, node) ->
        log ~title:"debug" "inspecting node: %s" (Browse_raw.string_of_node node);
        match node with
        | Browse_raw.Expression {exp_type = ty; _}
        | Pattern {pat_type = ty; _}
        | Core_type {ctyp_type = ty; _}
        | Value_description { val_desc = { ctyp_type = ty; _ }; _ } ->
          resolve_tlink env ty
        | _ -> None
      )
    in
    let locs = Std.List.filter_map path ~f:(fun (env, path) ->
      log ~title:"debug" "found type: %s" (Path.name path);
      let local_defs = Mtyper.get_typedtree typer in
      match
        Locate.from_string
          ~config:(Mpipeline.final_config pipeline)
          ~env ~local_defs ~pos ~namespaces:[`Type] `MLI
          (* FIXME: instead of converting to a string, pass it directly. *)
          (Path.name path)
      with
      | exception Env.Error _ -> None
      | `Found (path, lex_position) ->
        let position = position_of_lexical_position lex_position in
        let range = {Lsp.Protocol. start_ = position; end_ = position;} in
        let uri =
          match path with
          | None -> uri
          | Some path -> Lsp.Uri.of_path path
        in
        let loc = {Lsp.Protocol.Location. uri; range;} in
        Some loc
      | `At_origin
      | `Builtin _
      | `File_not_found _
      | `Invalid_context
      | `Missing_labels_namespace
      | `Not_found _
      | `Not_in_env _ -> None
    )
    in
    return (store, locs)

  | Lsp.Rpc.Request.TextDocumentCompletion {textDocument = {uri;}; position; context = _;} ->
    (* per LSP it requests completion with position after the prefix *)
    let position = {
      position with
      Lsp.Protocol.character = position.character - 1;
    } in
    let position = logical_of_position position in

    let make_string chars =
      let chars = Array.of_list chars in
      String.init (Array.length chars) (Array.get chars)
    in

    let prefix_of_position source position =
      match Msource.text source with
      | "" -> ""
      | text ->
        let len = String.length text in

        let rec find prefix i =
          if i < 0
          then make_string prefix
          else if i >= len
          then find prefix (i - 1)
          else
            let ch = text.[i] in
            match ch with
            | 'a'..'z'
            | 'A'..'Z'
            | '0'..'9'
            | '.'
            | '_' -> find (ch::prefix) (i - 1)
            | _ -> make_string prefix
        in

        let (`Offset index) = Msource.get_offset source position in
        find [] (index - 1)
    in

    Document_store.get store uri >>= fun doc ->
    let prefix = prefix_of_position (Document.source doc) position in
    log ~title:"debug" "completion prefix: |%s|" prefix;
    let command =
      Query_protocol.Complete_prefix (
        prefix,
        position,
        [
          `Constructor;
          `Labels;
          `Modules;
          `Modules_type;
          `Types;
          `Values;
          `Variants;
        ],
        true,
        true
      )
    in
    let completions = Query_commands.dispatch (Document.pipeline doc) command in
    let items =
      let f (entry : Query_protocol.Compl.entry) =
        {
          Lsp.Protocol.Completion.
          label = entry.name;
          kind = None;
          detail = Some entry.info;
          inlineDetail = None;
          itemType = None;
          documentation = None;
          sortText = None;
          filterText = None;
          insertText = None;
          insertTextFormat = None;
        }
      in
      List.map f completions.Query_protocol.Compl.entries
    in
    let resp = {Lsp.Protocol.Completion. isIncomplete = false; items;} in
    return (store, resp)
  | Lsp.Rpc.Request.UnknownRequest _ -> errorf "got unknown request"

let on_notification rpc store (notification : Lsp.Rpc.Client_notification.t) =
  let open Lsp.Utils.Result.Infix in

  match notification with

  | TextDocumentDidOpen params ->
    let notifications = ref [] in
    Logger.with_notifications notifications @@ fun () ->
      File_id.with_cache @@ fun () ->
        let doc =
          Document.make
            ~uri:params.textDocument.uri
            ~text:params.textDocument.text
            ()
        in
        Document_store.put store doc;
        send_diagnostics rpc doc;
        Ok store

  | TextDocumentDidChange {textDocument = {uri; version;}; contentChanges;}  ->
    Document_store.get store uri >>= fun prev_doc ->
    let doc =
      let f doc change = Document.update_text ~version change doc in
      List.fold_left f prev_doc contentChanges
    in
    Document_store.put store doc;
    send_diagnostics rpc doc;
    Ok store

  | Initialized -> Ok store
  | Exit -> Ok store

  | UnknownNotification ("$/setTraceNotification", _) -> Ok store
  | UnknownNotification ("$/cancelRequest", _) -> Ok store

  | UnknownNotification (id, json) ->
    log ~title:"warn" "unknown notification: %s %a"
      id
      (fun () -> Yojson.Safe.pretty_to_string ~std:false) json;
    Ok store

let start () =
  Lsp.Rpc.start (Document_store.make ()) {on_initialize; on_request; on_notification;} stdin stdout;
  log ~title:"info" "exiting"

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  match List.tl (Array.to_list Sys.argv) with
  | ("-help" | "--help" | "-h") :: _ ->
    Printf.eprintf
      "Usage: %s\nStart merlin LSP server (only stdio transport is supported)\n"
      Sys.argv.(0)
  | _ -> start ()

let () =
  let log_file =
    match Sys.getenv "MERLIN_LOG" with
    | exception Not_found -> None
    | file -> Some file
  in
  Logger.with_log_file ~sections:["ocamlmerlin-lsp"; "lsp";] log_file main
