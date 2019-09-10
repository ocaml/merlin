open Result
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
    referencesProvider = true;
    documentHighlightProvider = true;
    documentSymbolProvider = true;
    workspaceSymbolProvider = false;
    codeActionProvider = false;
    codeLensProvider = Some {
      codelens_resolveProvider = false;
    };
    documentFormattingProvider = false;
    documentRangeFormattingProvider = false;
    documentOnTypeFormattingProvider = None;
    renameProvider = true;
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
  val version : t -> int

  val update_text : ?version:int -> Lsp.Protocol.DidChange.textDocumentContentChangeEvent -> t -> t
end = struct
  type t = {
    tdoc : Lsp.Text_document.t;
    source : Msource.t;
    pipeline : Mpipeline.t;
    config : Mconfig.t;
  }

  let normalize_line_endings text =
    let text = Std.String.replace_all ~pattern:"\r\n" ~with_:"\n" text in
    text

  let uri doc = Lsp.Text_document.documentUri doc.tdoc
  let source doc = doc.source
  let pipeline doc = doc.pipeline
  let version doc = Lsp.Text_document.version doc.tdoc

  let make_config uri =
    let path = Lsp.Uri.to_path uri in
    let mconfig = Mconfig.initial in
    let path = Misc.canonicalize_filename path in
    let filename = Filename.basename path in
    let directory = Filename.dirname path in
    let mconfig = {
      mconfig with query = {
        mconfig.query with
        verbosity = 1;
        filename;
        directory;
      }
    } in
    Mconfig.load_dotmerlins mconfig ~filenames:[
      let base = "." ^ filename ^ ".merlin" in
      Filename.concat directory base
    ]

  let make ?(version=0) ~uri ~text () =
    let tdoc = Lsp.Text_document.make ~version uri text in
    (* we can do that b/c all text positions in LSP are line/col *)
    let text = normalize_line_endings text in
    let config = make_config uri in
    let source = Msource.make text in
    let pipeline = Mpipeline.make config source in
    {tdoc; source; config; pipeline;}

  let update_text ?version change doc =
    let tdoc = Lsp.Text_document.apply_content_change ?version change doc.tdoc in
    let text = Lsp.Text_document.text tdoc in
    log ~title:"debug" "TEXT\n%s" text;
    let config = make_config (Lsp.Text_document.documentUri tdoc) in
    let source = Msource.make text in
    let pipeline = Mpipeline.make config source in
    {tdoc; config; source; pipeline;}
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
  let col = position.character in
  `Logical (line, col)

let position_of_lexical_position (lex_position : Lexing.position) =
  let line = lex_position.pos_lnum - 1 in
  let character = lex_position.pos_cnum - lex_position.pos_bol in
  Lsp.Protocol.{line; character;}

let send_diagnostics rpc doc =
  let command =
    Query_protocol.Errors { lexing = true; parsing = true; typing = true }
  in
  let errors = Query_commands.dispatch (Document.pipeline doc) command in
  let diagnostics =
    List.map (fun (error : Location.error) ->
      let loc = Location.loc_of_report error in
      let range = {
        Lsp.Protocol.
        start_ = position_of_lexical_position loc.loc_start;
        end_ = position_of_lexical_position loc.loc_end;
      } in
      let severity =
        match error.source with
        | Warning -> Some Lsp.Protocol.PublishDiagnostics.Warning
        | _ -> Some Lsp.Protocol.PublishDiagnostics.Error
      in
      let message =
        Location.print_main Format.str_formatter error;
        String.trim (Format.flush_str_formatter ())
      in
      let diagnostic: Lsp.Protocol.PublishDiagnostics.diagnostic = {
        Lsp.Protocol.PublishDiagnostics.
        message;
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
    let query_type doc pos =
      let command = Query_protocol.Type_enclosing (None, pos, None) in
      match Query_commands.dispatch (Document.pipeline doc) command with
      | []
      | (_, `Index _, _) :: _ -> None
      | (location, `String value, _) :: _ -> Some (location, value)
    in

    let query_doc doc pos =
      let command = Query_protocol.Document (None, pos) in
      match Query_commands.dispatch (Document.pipeline doc) command with
      | `Found s | `Builtin s -> Some s
      | _ -> None
    in

    let format_contents ~as_markdown ~typ ~doc =
      let doc = match doc with None -> "" | Some s -> Printf.sprintf "\n(** %s *)" s in
      if as_markdown
      then {
        Lsp.Protocol.MarkupContent.
        value = Printf.sprintf "```ocaml\n%s%s\n```" typ doc;
        kind = Lsp.Protocol.MarkupKind.Markdown;
      }
      else {
        Lsp.Protocol.MarkupContent.
        value = Printf.sprintf "%s%s" doc typ;
        kind = Lsp.Protocol.MarkupKind.Plaintext;
      }
    in

    Document_store.get store uri >>= fun doc ->
    let pos = logical_of_position position in
    begin match query_type doc pos with
    | None -> return (store, None)
    | Some (loc, typ) ->
      let doc = query_doc doc pos in
      let as_markdown =
        List.mem
          Lsp.Protocol.MarkupKind.Markdown
          client_capabilities.textDocument.hover.contentFormat
      in
      let contents = format_contents ~as_markdown ~typ ~doc in
      let range = Some {
        Lsp.Protocol. start_ = position_of_lexical_position loc.Location.loc_start;
        end_ = position_of_lexical_position loc.loc_end;
      } in
      let resp = {
        Lsp.Protocol.Hover.
        contents;
        range;
      } in
      return (store, Some resp)
    end

  | Lsp.Rpc.Request.TextDocumentReferences {textDocument = {uri;}; position; context = _} ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Occurrences (`Ident_at (logical_of_position position)) in
    let locs : Location.t list = Query_commands.dispatch (Document.pipeline doc) command in
    let lsp_locs = List.map (fun loc ->
      let range = {
        Lsp.Protocol. start_ = position_of_lexical_position loc.Location.loc_start;
        end_ = position_of_lexical_position loc.loc_end;
      } in
      (* using original uri because merlin is looking only in local file *)
      {Lsp.Protocol.Location. uri; range;}
     ) locs in
    return (store, lsp_locs)

  | Lsp.Rpc.Request.TextDocumentCodeLens {textDocument = {uri;}} ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = Query_commands.dispatch (Document.pipeline doc) command in
    let symbol_infos =
      let rec symbol_info_of_outline_item item =
        let children =
          Std.List.concat_map
            item.Query_protocol.children
            ~f:symbol_info_of_outline_item
        in
        match item.Query_protocol.outline_type with
        | None -> children
        | Some typ ->
          let loc = item.Query_protocol.location in
          let info = {
            Lsp.Protocol.CodeLens.
            range = {
              start_ = position_of_lexical_position loc.loc_start;
              end_ = position_of_lexical_position loc.loc_end;
            };
            command = Some {
              Lsp.Protocol.Command.
              title = typ;
              command = "";
            };
          } in
          info::children
      in
      Std.List.concat_map ~f:symbol_info_of_outline_item outline
    in
    return (store, symbol_infos)

  | Lsp.Rpc.Request.TextDocumentHighlight {textDocument = {uri;}; position; } ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Occurrences (`Ident_at (logical_of_position position)) in
    let locs : Location.t list = Query_commands.dispatch (Document.pipeline doc) command in
    let lsp_locs = List.map (fun loc ->
      let range = {
        Lsp.Protocol. start_ = position_of_lexical_position loc.Location.loc_start;
        end_ = position_of_lexical_position loc.loc_end;
      } in
      (* using the default kind as we are lacking info
         to make a difference between assignment and usage. *)
      {Lsp.Protocol.DocumentHighlight. kind = Some Text; range;}
     ) locs in
    return (store, lsp_locs)

  | Lsp.Rpc.Request.DocumentSymbol {textDocument = {uri;}} ->
    let module DocumentSymbol = Lsp.Protocol.DocumentSymbol in
    let module SymbolKind = Lsp.Protocol.SymbolKind in

    let kind item =
      match item.Query_protocol.outline_kind with
      | `Value -> SymbolKind.Function
      | `Constructor -> SymbolKind.Constructor
      | `Label -> SymbolKind.Property
      | `Module -> SymbolKind.Module
      | `Modtype -> SymbolKind.Module
      | `Type -> SymbolKind.String
      | `Exn -> SymbolKind.Constructor
      | `Class -> SymbolKind.Class
      | `Method -> SymbolKind.Method
    in

    let range item = {
      Lsp.Protocol.
      start_ = position_of_lexical_position item.Query_protocol.location.loc_start;
      end_ = position_of_lexical_position item.location.loc_end;
    } in

    let rec symbol item =
      let children = Std.List.map item.Query_protocol.children ~f:symbol in
      let range = range item in
      {
        Lsp.Protocol.DocumentSymbol.
        name = item.Query_protocol.outline_name;
        detail = item.Query_protocol.outline_type;
        kind = kind item;
        deprecated = false;
        range = range;
        selectionRange = range;
        children;
      }
    in

    let rec symbol_info ?containerName item =
      let location = {
        Lsp.Protocol.Location.
        uri;
        range = range item;
      } in
      let info = {
        Lsp.Protocol.SymbolInformation.
        name = item.Query_protocol.outline_name;
        kind = kind item;
        deprecated = false;
        location;
        containerName;
      } in
      let children =
        Std.List.concat_map
          item.children
          ~f:(symbol_info ~containerName:info.name)
      in
      info::children
    in

    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Outline in
    let outline = Query_commands.dispatch (Document.pipeline doc) command in
    let symbols =
      let caps = client_capabilities.textDocument.documentSymbol in
      match caps.hierarchicalDocumentSymbolSupport with
      | true ->
        let symbols = Std.List.map outline ~f:symbol in
        Lsp.Protocol.TextDocumentDocumentSymbol.DocumentSymbol symbols
      | false ->
        let symbols = Std.List.concat_map ~f:symbol_info outline in
        Lsp.Protocol.TextDocumentDocumentSymbol.SymbolInformation symbols
    in
    return (store, symbols)

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
    let lsp_position = position in
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
            (* The characters for an infix function are missing *)
            match ch with
            | 'a'..'z'
            | 'A'..'Z'
            | '0'..'9'
            | '.'
            | '\''
            | '_' -> find (ch::prefix) (i - 1)
            | _ -> make_string prefix
        in

        let (`Offset index) = Msource.get_offset source position in
        find [] (index - 1)
    in

    let range_prefix prefix =
      let start_ =
        let len = String.length prefix in
        let character = lsp_position.character - len in
        { lsp_position with character }
      in
      { Lsp.Protocol.
        start_;
        end_ = lsp_position;
      }
    in

    let item index entry =
      let prefix, (entry: Query_protocol.Compl.entry) =
        match entry with
        | `Keep entry -> `Keep, entry
        | `Replace (range, entry) -> `Replace range, entry
      in
      let kind: Lsp.Protocol.Completion.completionItemKind option =
        match entry.kind with
        | `Value -> Some Value
        | `Constructor -> Some Constructor
        | `Variant -> None
        | `Label -> Some Property
        | `Module |`Modtype -> Some Module
        | `Type -> Some TypeParameter
        | `MethodCall -> Some Method
      in
      let textEdit =
        match prefix with
        | `Keep -> None
        | `Replace range -> Some {
          Lsp.Protocol.TextEdit.
          range;
          newText = entry.name;
        }
      in
      {
        Lsp.Protocol.Completion.
        label = entry.name;
        kind;
        detail = Some entry.desc;
        inlineDetail = None;
        itemType = Some entry.desc;
        documentation = Some entry.info;
        (* Without this field the client is not forced to
           respect the order provided by merlin. *)
        sortText = Some (Printf.sprintf "%04d" index);
        filterText = None;
        insertText = None;
        insertTextFormat = None;
        textEdit;
        additionalTextEdits = [];
      }
    in

    let completion_kinds =
        [
          `Constructor;
          `Labels;
          `Modules;
          `Modules_type;
          `Types;
          `Values;
          `Variants;
        ]
    in

    Document_store.get store uri >>= fun doc ->
    let prefix = prefix_of_position (Document.source doc) position in
    log ~title:"debug" "completion prefix: |%s|" prefix;
    let complete =
      Query_protocol.Complete_prefix (
        prefix,
        position,
        completion_kinds,
        true,
        true
      )
    in
    let expand =
      Query_protocol.Expand_prefix (
        prefix,
        position,
        completion_kinds,
        true
      )
    in

    let completions = Query_commands.dispatch (Document.pipeline doc) complete in
    let labels =
      match completions with
      | { Query_protocol.Compl. entries = _; context = `Unknown } -> []
      | { Query_protocol.Compl. entries = _; context = `Application context } ->
        let { Query_protocol.Compl. labels; argument_type = _ } = context in
        List.map (fun (name, typ) ->
          `Keep {
            Query_protocol.Compl.
            name;
            kind = `Label;
            desc = typ;
            info = "";
          }
        ) labels
    in
    let items =
      match completions, labels with
      | { Query_protocol.Compl. entries = []; context = _ }, [] ->
        let { Query_protocol.Compl. entries; context = _} =
          Query_commands.dispatch (Document.pipeline doc) expand
        in
        let range = range_prefix prefix in
        List.map (fun entry -> (`Replace (range, entry))) entries
      | { entries; context = _ }, _labels ->
        List.map (fun entry -> `Keep entry) entries
    in
    let all = List.concat [labels; items] in
    let items = List.mapi item all in
    let resp = {Lsp.Protocol.Completion. isIncomplete = false; items;} in
    return (store, resp)

  | Lsp.Rpc.Request.TextDocumentRename { textDocument = { uri }; position; newName } ->
    Document_store.get store uri >>= fun doc ->
    let command = Query_protocol.Occurrences (`Ident_at (logical_of_position position)) in
    let locs : Location.t list = Query_commands.dispatch (Document.pipeline doc) command in
    let version = Document.version doc in
    let edits = List.map (fun loc ->
      let range =
        {
          Lsp.Protocol. start_ = position_of_lexical_position loc.Location.loc_start;
          end_ = position_of_lexical_position loc.loc_end;
        }
      in
      {Lsp.Protocol.TextEdit. newText = newName; range;}
    ) locs
    in
    let workspace_edits =
      let documentChanges = client_capabilities.workspace.workspaceEdit.documentChanges in
      Lsp.Protocol.WorkspaceEdit.make ~documentChanges ~uri ~version ~edits
    in
    return (store, workspace_edits)
  | Lsp.Rpc.Request.UnknownRequest _ -> errorf "got unknown request"

let on_notification rpc store (notification : Lsp.Rpc.Client_notification.t) =
  let open Lsp.Utils.Result.Infix in
  match notification with

  | TextDocumentDidOpen params ->
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
  let docs = Document_store.make () in
  let prepare_and_run f =
    (* TODO: what to do with merlin notifications? *)
    let _notifications = ref [] in
    Logger.with_notifications (ref []) @@ fun () ->
    File_id.with_cache @@ f
  in
  let on_initialize rpc state params =
    prepare_and_run @@ fun () ->
    on_initialize rpc state params
  in
  let on_notification rpc state notif =
    prepare_and_run @@ fun () ->
    on_notification rpc state notif
  in
  let on_request rpc state caps req =
    prepare_and_run @@ fun () ->
    on_request rpc state caps req
  in
  Lsp.Rpc.start
    docs
    {on_initialize; on_request; on_notification;}
    stdin
    stdout;
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
