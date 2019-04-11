(**
 * This encodes LSP protocol specification as document at
 *
 *   https://microsoft.github.io/language-server-protocol/specification
 *
 * Most of this was borrowed from facebook/flow repository.
 *
 *)

open Protocol_conv_json
module Json = Json.Make(
  struct
    let field_name str = str
    let singleton_constr_as_string = false
    let omit_default_values = true
  end)

type documentUri = Uri.t [@@deriving protocol ~driver:(module Json)]

type zero_based_int = int [@@deriving protocol ~driver:(module Json)]

type position = {
  line: zero_based_int;
  character: zero_based_int;
} [@@deriving protocol ~driver:(module Json)]

type range = {
  start_: position [@key "start"];
  end_: position [@key "end"];
} [@@deriving protocol ~driver:(module Json)]

module Command = struct
  type t = {
    title : string;
    command : string;
  } [@@deriving protocol ~driver:(module Json)]
end

module MarkupKind = struct
  type t =
    | Plaintext
    | Markdown
      [@@deriving protocol ~driver:(module Json)]
end

module MarkupContent = struct
  type t = {
    value: string;
    kind: MarkupKind.t;
  } [@@deriving protocol ~driver:(module Json)]
end

module Location = struct
  type t = {
    uri: Uri.t;
    range : range;
  } [@@deriving protocol ~driver:(module Json)]
end

module DefinitionLocation = struct
  type t = {
    uri: Uri.t;
    range : range;
    title: string option [@default None];
  } [@@deriving protocol ~driver:(module Json)]
end

(* Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
  } [@@deriving protocol ~driver:(module Json)]
end

(* An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    version: int;  (* the version number of this document *)
  } [@@deriving protocol ~driver:(module Json)]
end

(* An item to transfer a text document from the client to the server. The
   version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    languageId: string;  (* the text document's language identifier *)
    version: int;  (* the version of the document *)
    text: string;  (* the content of the opened text document *)
  } [@@deriving protocol ~driver:(module Json)]
end

(* DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams [@@deriving protocol ~driver:(module Json)]

  and didOpenTextDocumentParams = {
    textDocument: TextDocumentItem.t;  (* the document that was opened *)
  }
end

(* DidChangeTextDocument notification, method="textDocument/didChange" *)
module DidChange = struct
  type params = didChangeTextDocumentParams [@@deriving protocol ~driver:(module Json)]

  and didChangeTextDocumentParams = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanges: textDocumentContentChangeEvent list;
  }

  and textDocumentContentChangeEvent = {
    range: range option [@default None]; (* the range of the document that changed *)
    rangeLength: int option [@default None]; (* the length that got replaced *)
    text: string; (* the new text of the range/document *)
  }
end

module TextDocumentPositionParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
  } [@@deriving protocol ~driver:(module Json)]
end

(**
  A document highlight is a range inside a text document which deserves
  special attention. Usually a document highlight is visualized by changing
  the background color of its range.
*)
module DocumentHighlight = struct

  (** The highlight kind, default is DocumentHighlightKind.Text. *)
  type kind =
    | Text (** 1: A textual occurrence. *)
    | Read (** 2: Read-access of a symbol, like reading a variable. *)
    | Write (** 3: Write-access of a symbol, like writing a variable. *)

  let kind_to_json = function
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let kind_of_json = function
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | _ -> invalid_arg "unable to read the kind"

  type t = {
    range: range;
    kind: kind option;
  } [@@deriving protocol ~driver:(module Json)]

end

(**
   Complex text manipulations are described with an array of
   TextEdit's, representing a single change to the document.

   All text edits ranges refer to positions in the original
   document. Text edits ranges must never overlap, that means no part of
   the original document must be manipulated by more than one
   edit. However, it is possible that multiple edits have the same start
   position: multiple inserts, or any number of inserts followed by a
   single remove or replace edit. If multiple inserts have the same
   position, the order in the array defines the order in which the
   inserted strings appear in the resulting text.
*)
module TextEdit = struct
  type t = {
    (** The range of the text document to be manipulated. To insert text into
        a document create a range where start === end. *)
    range: range;
    (** The string to be inserted. For delete operations use an empty string. *)
    newText: string;
  } [@@deriving protocol ~driver:(module Json)]
end


(**
   Describes textual changes on a single text document. The text
   document is referred to as a VersionedTextDocumentIdentifier to
   allow clients to check the text document version before an edit is
   applied. A TextDocumentEdit describes all changes on a version Si
   and after they are applied move the document to version Si+1. So
   the creator of a TextDocumentEdit doesn't need to sort the array or
   do any kind of ordering. However the edits must be non overlapping.
*)
module TextDocumentEdit = struct
  type t = {
    textDocument: VersionedTextDocumentIdentifier.t; (** The text document to change. *)
    edits: TextEdit.t list; (** The edits to be applied. *)
  } [@@deriving protocol ~driver:(module Json)]
end

(**
   A workspace edit represents changes to many resources managed in
   the workspace. The edit should either provide [changes] or
   [documentChanges]. If the client can handle versioned document edits
   and if [documentChanges] are present, the latter are preferred over
   [changes].
*)
module WorkspaceEdit = struct

  (** Holds changes to existing resources.

      The json representation is an object with URIs as keys and edits
      as values.
  *)
  type changes = (documentUri * TextEdit.t list) list

  let changes_to_json changes =
    let changes =
      List.map (fun (uri, edits) ->
        let uri = Uri.to_string uri in
        let edits = `List (List.map TextEdit.to_json edits) in
        uri, edits
      ) changes
    in
    `Assoc changes

  type documentChanges = TextDocumentEdit.t list [@@deriving to_protocol ~driver:(module Json)]

  (**
     Depending on the client capability
     [workspace.workspaceEdit.resourceOperations] document changes are either an
     array of [TextDocumentEdit]s to express changes to n different text
     documents where each text document edit addresses a specific version of a
     text document. Or it can contain above [TextDocumentEdit]s mixed with
     create, rename and delete file / folder operations.

     Whether a client supports versioned document edits is expressed via
     [workspace.workspaceEdit.documentChanges] client capability.

     If a client neither supports [documentChanges] nor
     [workspace.workspaceEdit.resourceOperations] then only plain [TextEdit]s
     using the [changes] property are supported.
  *)
  type t = {
    changes: changes option;
    documentChanges: documentChanges option;
  } [@@deriving to_protocol ~driver:(module Json)]

  let empty = {
    changes = None;
    documentChanges = None;
  }

  (** Create a {!type:t} based on the capabilites of the client. *)
  let make ~documentChanges ~uri ~version ~edits =
    match documentChanges with
    | false ->
      let changes = Some [ uri, edits ] in
      { empty with changes }
    | true ->
      let documentChanges =
        let textDocument = {
          VersionedTextDocumentIdentifier.
          uri;
          version;
        }
        in
        let edits = {
          TextDocumentEdit.
          edits;
          textDocument;
        }
        in
        Some [edits]
      in
      { empty with documentChanges }
end

(* PublishDiagnostics notification, method="textDocument/PublishDiagnostics" *)
module PublishDiagnostics = struct
  type diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  let diagnosticCode_to_json = function
    | IntCode v -> `Int v
    | StringCode v -> `String v
    | NoCode -> `Null

  let diagnosticCode_of_json = function
    | `Int v -> (IntCode v)
    | `String v -> (StringCode v)
    | `Null -> NoCode
    | _ -> invalid_arg "invalid diagnostic.code"

  type diagnosticSeverity =
    | Error (* 1 *)
    | Warning (* 2 *)
    | Information (* 3 *)
    | Hint (* 4 *)

  let diagnosticSeverity_to_json = function
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let diagnosticSeverity_of_json = function
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | _ -> invalid_arg "expected int"

  type params = publishDiagnosticsParams [@@deriving protocol ~driver:(module Json)]

  and publishDiagnosticsParams = {
    uri: documentUri;
    diagnostics: diagnostic list;
  }

  and diagnostic = {
    range: range;  (* the range at which the message applies *)
    severity: diagnosticSeverity option [@default None];  (* if omitted, client decides *)
    code: diagnosticCode [@default NoCode];  (* the diagnostic's code. *)
    source: string option [@default None];  (* human-readable string, eg. typescript/lint *)
    message: string;  (* the diagnostic's message *)
    relatedInformation: diagnosticRelatedInformation list;
    relatedLocations: relatedLocation list; (* legacy FB extension *)
  }

  and diagnosticRelatedInformation = {
    relatedLocation: Location.t;  (* wire: just "location" *)
    relatedMessage: string;  (* wire: just "message" *)
  }

  (* legacy FB extension *)
  and relatedLocation = diagnosticRelatedInformation
end

(* Completion request, method="textDocument/completion" *)
module Completion = struct

  type completionTriggerKind =
    | Invoked (* 1 *)
    | TriggerCharacter (* 2 *)
    | TriggerForIncompleteCompletions (* 3 *)

  let completionTriggerKind_to_json = function
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let completionTriggerKind_of_json = function
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | _ -> invalid_arg "invalid completion.triggerKind"

  type completionItemKind =
    | Text (* 1 *)
    | Method (* 2 *)
    | Function (* 3 *)
    | Constructor (* 4 *)
    | Field (* 5 *)
    | Variable (* 6 *)
    | Class (* 7 *)
    | Interface (* 8 *)
    | Module (* 9 *)
    | Property (* 10 *)
    | Unit (* 11 *)
    | Value (* 12 *)
    | Enum (* 13 *)
    | Keyword (* 14 *)
    | Snippet (* 15 *)
    | Color (* 16 *)
    | File (* 17 *)
    | Reference (* 18 *)
    | Folder (* 19 *)
    | EnumMember (* 20 *)
    | Constant (* 21 *)
    | Struct (* 22 *)
    | Event (* 23 *)
    | Operator (* 24 *)
    | TypeParameter (* 25 *)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with completionItemKind_of_int_opt. *)
  let int_of_completionItemKind = function
    | Text -> 1
    | Method -> 2
    | Function -> 3
    | Constructor -> 4
    | Field -> 5
    | Variable -> 6
    | Class -> 7
    | Interface -> 8
    | Module -> 9
    | Property -> 10
    | Unit -> 11
    | Value -> 12
    | Enum -> 13
    | Keyword -> 14
    | Snippet -> 15
    | Color -> 16
    | File -> 17
    | Reference -> 18
    | Folder -> 19
    | EnumMember -> 20
    | Constant -> 21
    | Struct -> 22
    | Event -> 23
    | Operator -> 24
    | TypeParameter -> 25

  let completionItemKind_to_json v =
    `Int (int_of_completionItemKind v)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with int_of_completionItemKind. *)
  let completionItemKind_of_int_opt = function
    | 1 -> Some Text
    | 2 -> Some Method
    | 3 -> Some Function
    | 4 -> Some Constructor
    | 5 -> Some Field
    | 6 -> Some Variable
    | 7 -> Some Class
    | 8 -> Some Interface
    | 9 -> Some Module
    | 10 -> Some Property
    | 11 -> Some Unit
    | 12 -> Some Value
    | 13 -> Some Enum
    | 14 -> Some Keyword
    | 15 -> Some Snippet
    | 16 -> Some Color
    | 17 -> Some File
    | 18 -> Some Reference
    | 19 -> Some Folder
    | 20 -> Some EnumMember
    | 21 -> Some Constant
    | 22 -> Some Struct
    | 23 -> Some Event
    | 24 -> Some Operator
    | 25 -> Some TypeParameter
    | _ -> None

  let completionItemKind_of_json = function
    | `Int v ->
      begin match completionItemKind_of_int_opt v with
      | Some v -> v
      | None -> invalid_arg "invalid completion.kind"
      end
    | _ -> invalid_arg "invalid completion.kind: expected an integer"

    (** Keep this in sync with `int_of_completionItemKind`. *)
  type insertTextFormat =
    | PlainText (* 1 *)  (* the insertText/textEdits are just plain strings *)
    | SnippetFormat (* 2 *)  (* wire: just "Snippet" *)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with insertFormat_of_int_opt. *)
  let int_of_insertFormat = function
    | PlainText -> 1
    | SnippetFormat -> 2

  let insertTextFormat_to_json v =
    `Int (int_of_insertFormat v)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with int_of_insertFormat. *)
  let insertFormat_of_int_opt = function
    | 1 -> Some PlainText
    | 2 -> Some SnippetFormat
    | _ -> None

  let insertTextFormat_of_json = function
    | `Int v ->
      begin match insertFormat_of_int_opt v with
      | Some v -> v
      | None -> invalid_arg "invalid completion.kind"
      end
    | _ -> invalid_arg "invalid completion.kind: expected an integer"

  type params = completionParams [@@deriving protocol ~driver:(module Json)]

  and completionParams = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: completionContext option [@default None];
  }

  and completionContext = {
    triggerKind: completionTriggerKind;
  }

  and result = completionList  (* wire: can also be 'completionItem list' *)

  and completionList = {
    isIncomplete: bool; (* further typing should result in recomputing *)
    items: completionItem list;
  }

  and completionItem = {
    label: string;  (* the label in the UI *)
    kind: completionItemKind option [@default None];  (* tells editor which icon to use *)
    detail: string option [@default None];  (* human-readable string like type/symbol info *)
    inlineDetail: string option [@default None]; (* nuclide-specific, right column *)
    itemType: string option [@default None]; (* nuclide-specific, left column *)
    documentation: string option [@default None];  (* human-readable doc-comment *)
    sortText: string option [@default None];  (* used for sorting; if absent, uses label *)
    filterText: string option [@default None];  (* used for filtering; if absent, uses label *)
    insertText: string option [@default None];  (* used for inserting; if absent, uses label *)
    insertTextFormat: insertTextFormat option [@default None];
    (* textEdits: TextEdit.t list;  (1* wire: split into hd and tl *1) *)
    (* command: Command.t option [@default None];  (1* if present, is executed after completion *1) *)
    (* data: Hh_json.json option [@default None]; *)
  }
end

(* Hover request, method="textDocument/hover" *)
module Hover = struct
  type params =
    TextDocumentPositionParams.t
    [@@deriving protocol ~driver:(module Json)]

  and result = hoverResult option [@default None]

  and hoverResult = {
    contents: MarkupContent.t;
    range: range option [@default None];
  }
end

(* Initialize request, method="initialize" *)
module Initialize = struct

  type trace =
    | Off [@name "off"]
    | Messages [@name "messages"]
    | Verbose [@name "verbose"]
    [@@deriving protocol ~driver:(module Json)]

  type textDocumentSyncKind =
    | NoSync (* 0 *)  (* docs should not be synced at all. Wire "None" *)
    | FullSync (* 1 *)  (* synced by always sending full content. Wire "Full" *)
    | IncrementalSync (* 2 *)  (* full only on open. Wire "Incremental" *)

  let textDocumentSyncKind_to_json = function
    | NoSync -> `Int 0
    | FullSync -> `Int 1
    | IncrementalSync -> `Int 2

  let textDocumentSyncKind_of_json = function
    | `Int 0 -> NoSync
    | `Int 1 -> FullSync
    | `Int 2 -> IncrementalSync
    | _ -> invalid_arg "invalid textDocumentSyncKind"

  (* synchronization capabilities say what messages the client is capable
   * of sending, should be be so asked by the server.
   * We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)

  type synchronization = {
    willSave: bool;  (* client can send textDocument/willSave *)
    willSaveWaitUntil: bool;  (* textDoc.../willSaveWaitUntil *)
    didSave: bool;  (* textDocument/didSave *)
  } [@@deriving protocol ~driver:(module Json)]

  let synchronization_empty = {
    willSave = true;
    willSaveWaitUntil = true;
    didSave = true;
  }

  type completionItem = {
    snippetSupport: bool;  (* client can do snippets as insert text *)
  } [@@deriving protocol ~driver:(module Json)]

  let completionItem_empty = {
    snippetSupport = false;
  }

  type completion = {
    completionItem: completionItem [@default completionItem_empty];
  } [@@deriving protocol ~driver:(module Json)]

  let completion_empty = {
    completionItem = completionItem_empty;
  }

  type hover = {
    contentFormat: MarkupKind.t list [@default [Plaintext]];
  } [@@deriving protocol ~driver:(module Json)]

  let hover_empty = {
    contentFormat = [Plaintext];
  }

  type documentSymbol = {
    hierarchicalDocumentSymbolSupport : bool [@default false];
  } [@@deriving protocol ~driver:(module Json)]

  let documentSymbol_empty = {
    hierarchicalDocumentSymbolSupport = false;
  }

  type textDocumentClientCapabilities = {
    synchronization: synchronization [@default synchronization_empty];
    (** textDocument/completion *)
    completion: completion [@default completion_empty];
    (** textDocument/documentSymbol *)
    documentSymbol: documentSymbol [@default documentSymbol_empty];
    (** textDocument/hover *)
    hover: hover [@default hover_empty];
    (* omitted: dynamic-registration fields *)
  } [@@deriving protocol ~driver:(module Json)]

  let textDocumentClientCapabilities_empty = {
    completion = completion_empty;
    synchronization = synchronization_empty;
    hover = hover_empty;
    documentSymbol = documentSymbol_empty;
  }

  type workspaceEdit = {
    documentChanges: bool;  (* client supports versioned doc changes *)
  } [@@deriving protocol ~driver:(module Json)]

  let workspaceEdit_empty = {
    documentChanges = false;
  }

  type workspaceClientCapabilities = {
    applyEdit: bool [@default false];  (* client supports appling batch edits *)
    workspaceEdit: workspaceEdit [@default workspaceEdit_empty];
    (* omitted: dynamic-registration fields *)
  } [@@deriving protocol ~driver:(module Json)]

  let workspaceClientCapabilities_empty = {
    applyEdit = false;
    workspaceEdit = workspaceEdit_empty;
  }

  type windowClientCapabilities = {
    status: bool;  (* Nuclide-specific: client supports window/showStatusRequest *)
    progress: bool;  (* Nuclide-specific: client supports window/progress *)
    actionRequired: bool;  (* Nuclide-specific: client supports window/actionRequired *)
  } [@@deriving protocol ~driver:(module Json)]

  let windowClientCapabilities_empty = {
    status = true;
    progress = true;
    actionRequired = true;
  }

  type telemetryClientCapabilities = {
    connectionStatus: bool;  (* Nuclide-specific: client supports telemetry/connectionStatus *)
  } [@@deriving protocol ~driver:(module Json)]

  let telemetryClientCapabilities_empty = {
    connectionStatus = true;
  }

  type client_capabilities = {
    workspace: workspaceClientCapabilities [@default workspaceClientCapabilities_empty];
    textDocument: textDocumentClientCapabilities [@default textDocumentClientCapabilities_empty];
    window: windowClientCapabilities [@default windowClientCapabilities_empty];
    telemetry: telemetryClientCapabilities [@default telemetryClientCapabilities_empty];
    (* omitted: experimental *)
  } [@@deriving protocol ~driver:(module Json)]

  let client_capabilities_empty = {
    workspace = workspaceClientCapabilities_empty;
    textDocument = textDocumentClientCapabilities_empty;
    window = windowClientCapabilities_empty;
    telemetry = telemetryClientCapabilities_empty;
  }

  type params = {
    processId: int option [@default None];  (* pid of parent process *)
    rootPath: string option [@default None];  (* deprecated *)
    rootUri: documentUri option [@default None];  (* the root URI of the workspace *)
    client_capabilities: client_capabilities [@key "capabilities"];
    trace: trace [@default Off];  (* the initial trace setting, default="off" *)
  } [@@deriving_inline protocol ~driver:(module Json)]

  and result = {
    server_capabilities: server_capabilities [@key "capabilities"];
  }

  and errorData = {
    retry: bool;  (* should client retry the initialize request *)
  }

  (* What capabilities the server provides *)
  and server_capabilities = {
    textDocumentSync: textDocumentSyncOptions; (* how to sync *)
    hoverProvider: bool;
    completionProvider: completionOptions option [@default None];
    (* signatureHelpProvider: signatureHelpOptions option; *)
    definitionProvider: bool;
    typeDefinitionProvider: bool;
    referencesProvider: bool;
    documentHighlightProvider: bool;
    documentSymbolProvider: bool;  (* ie. document outline *)
    workspaceSymbolProvider: bool;  (* ie. find-symbol-in-project *)
    codeActionProvider: bool;
    codeLensProvider: codeLensOptions option [@default None];
    documentFormattingProvider: bool;
    documentRangeFormattingProvider: bool;
    documentOnTypeFormattingProvider: documentOnTypeFormattingOptions option [@default None];
    renameProvider: bool;
    documentLinkProvider: documentLinkOptions option [@default None];
    executeCommandProvider: executeCommandOptions option [@default None];
    typeCoverageProvider: bool;  (* Nuclide-specific feature *)
    rageProvider: bool;
    (* omitted: experimental *)
  }

  and completionOptions = {
    resolveProvider: bool;  (* server resolves extra info on demand *)
    triggerCharacters: string list; (* wire "triggerCharacters" *)
  }

  (* and signatureHelpOptions = { *)
  (*   sighelp_triggerCharacters: string list; (1* wire "triggerCharacters" *1) *)
  (* } *)

  and codeLensOptions = {
    codelens_resolveProvider: bool [@key "resolveProvider"];  (* wire "resolveProvider" *)
  }

  and documentOnTypeFormattingOptions = {
    firstTriggerCharacter: string;  (* e.g. "}" *)
    moreTriggerCharacter: string list;
  }

  and documentLinkOptions = {
    doclink_resolveProvider: bool;  (* wire "resolveProvider" *)
  }

  and executeCommandOptions = {
    commands: string list;  (* the commands to be executed on the server *)
  }

  (* text document sync options say what messages the server requests the
   * client to send. We use the "want_" prefix for OCaml naming reasons;
   * this prefix is absent in LSP. *)
  and textDocumentSyncOptions = {
    openClose: bool;  (* textDocument/didOpen+didClose *)
    change: textDocumentSyncKind;
    willSave: bool;  (* textDocument/willSave *)
    willSaveWaitUntil: bool;  (* textDoc.../willSaveWaitUntil *)
    didSave: saveOptions option [@default None];  (* textDocument/didSave *)
  }

  and saveOptions = {
    includeText: bool;  (* the client should include content on save *)
    }
 
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let _ = fun (_ : errorData) -> ()
let _ = fun (_ : server_capabilities) -> ()
let _ = fun (_ : completionOptions) -> ()
let _ = fun (_ : codeLensOptions) -> ()
let _ = fun (_ : documentOnTypeFormattingOptions) -> ()
let _ = fun (_ : documentLinkOptions) -> ()
let _ = fun (_ : executeCommandOptions) -> ()
let _ = fun (_ : textDocumentSyncOptions) -> ()
let _ = fun (_ : saveOptions) -> ()
let rec params_to_json t =
  (fun { processId; rootPath; rootUri; client_capabilities; trace } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("processId", processId,
               ((fun t -> Json.of_option t) (fun t -> Json.of_int t)),
               (Some None)),
              (Cons
                 (("rootPath", rootPath,
                    ((fun t -> Json.of_option t) (fun t -> Json.of_string t)),
                    (Some None)),
                   (Cons
                      (("rootUri", rootUri,
                         ((fun t -> Json.of_option t) documentUri_to_json),
                         (Some None)),
                        (Cons
                           (("capabilities", client_capabilities,
                              client_capabilities_to_json, None),
                             (Cons
                                (("trace", trace, trace_to_json, (Some Off)),
                                  Nil))))))))))) t[@@ocaml.warning "-39"]
and result_to_json t =
  (fun { server_capabilities } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("capabilities", server_capabilities,
               server_capabilities_to_json, None), Nil))) t[@@ocaml.warning
                                                             "-39"]
and errorData_to_json t =
  (fun { retry } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons (("retry", retry, (fun t -> Json.of_bool t), None), Nil))) t
[@@ocaml.warning "-39"]
and server_capabilities_to_json t =
  (fun
     { textDocumentSync; hoverProvider; completionProvider;
       definitionProvider; typeDefinitionProvider; referencesProvider;
       documentHighlightProvider; documentSymbolProvider;
       workspaceSymbolProvider; codeActionProvider; codeLensProvider;
       documentFormattingProvider; documentRangeFormattingProvider;
       documentOnTypeFormattingProvider; renameProvider;
       documentLinkProvider; executeCommandProvider; typeCoverageProvider;
       rageProvider }
     ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("textDocumentSync", textDocumentSync,
               textDocumentSyncOptions_to_json, None),
              (Cons
                 (("hoverProvider", hoverProvider, (fun t -> Json.of_bool t),
                    None),
                   (Cons
                      (("completionProvider", completionProvider,
                         ((fun t -> Json.of_option t)
                            completionOptions_to_json), (Some None)),
                        (Cons
                           (("definitionProvider", definitionProvider,
                              (fun t -> Json.of_bool t), None),
                             (Cons
                                (("typeDefinitionProvider",
                                   typeDefinitionProvider,
                                   (fun t -> Json.of_bool t), None),
                                  (Cons
                                     (("referencesProvider",
                                        referencesProvider,
                                        (fun t -> Json.of_bool t), None),
                                       (Cons
                                          (("documentHighlightProvider",
                                             documentHighlightProvider,
                                             (fun t -> Json.of_bool t), None),
                                            (Cons
                                               (("documentSymbolProvider",
                                                  documentSymbolProvider,
                                                  (fun t -> Json.of_bool t),
                                                  None),
                                                 (Cons
                                                    (("workspaceSymbolProvider",
                                                       workspaceSymbolProvider,
                                                       (fun t ->
                                                          Json.of_bool t),
                                                       None),
                                                      (Cons
                                                         (("codeActionProvider",
                                                            codeActionProvider,
                                                            (fun t ->
                                                               Json.of_bool t),
                                                            None),
                                                           (Cons
                                                              (("codeLensProvider",
                                                                 codeLensProvider,
                                                                 ((fun t ->
                                                                    Json.of_option
                                                                    t)
                                                                    codeLensOptions_to_json),
                                                                 (Some None)),
                                                                (Cons
                                                                   (("documentFormattingProvider",
                                                                    documentFormattingProvider,
                                                                    (fun t ->
                                                                    Json.of_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("documentRangeFormattingProvider",
                                                                    documentRangeFormattingProvider,
                                                                    (fun t ->
                                                                    Json.of_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("documentOnTypeFormattingProvider",
                                                                    documentOnTypeFormattingProvider,
                                                                    ((fun t
                                                                    ->
                                                                    Json.of_option
                                                                    t)
                                                                    documentOnTypeFormattingOptions_to_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("renameProvider",
                                                                    renameProvider,
                                                                    (fun t ->
                                                                    Json.of_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("documentLinkProvider",
                                                                    documentLinkProvider,
                                                                    ((fun t
                                                                    ->
                                                                    Json.of_option
                                                                    t)
                                                                    documentLinkOptions_to_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("executeCommandProvider",
                                                                    executeCommandProvider,
                                                                    ((fun t
                                                                    ->
                                                                    Json.of_option
                                                                    t)
                                                                    executeCommandOptions_to_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("typeCoverageProvider",
                                                                    typeCoverageProvider,
                                                                    (fun t ->
                                                                    Json.of_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("rageProvider",
                                                                    rageProvider,
                                                                    (fun t ->
                                                                    Json.of_bool
                                                                    t), None),
                                                                    Nil)))))))))))))))))))))))))))))))))))))))
    t[@@ocaml.warning "-39"]
and completionOptions_to_json t =
  (fun { resolveProvider; triggerCharacters } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("resolveProvider", resolveProvider, (fun t -> Json.of_bool t),
               None),
              (Cons
                 (("triggerCharacters", triggerCharacters,
                    ((fun t -> Json.of_list t) (fun t -> Json.of_string t)),
                    None), Nil))))) t[@@ocaml.warning "-39"]
and codeLensOptions_to_json t =
  (fun { codelens_resolveProvider } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("resolveProvider", codelens_resolveProvider,
               (fun t -> Json.of_bool t), None), Nil))) t[@@ocaml.warning
                                                           "-39"]
and documentOnTypeFormattingOptions_to_json t =
  (fun { firstTriggerCharacter; moreTriggerCharacter } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("firstTriggerCharacter", firstTriggerCharacter,
               (fun t -> Json.of_string t), None),
              (Cons
                 (("moreTriggerCharacter", moreTriggerCharacter,
                    ((fun t -> Json.of_list t) (fun t -> Json.of_string t)),
                    None), Nil))))) t[@@ocaml.warning "-39"]
and documentLinkOptions_to_json t =
  (fun { doclink_resolveProvider } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("doclink_resolveProvider", doclink_resolveProvider,
               (fun t -> Json.of_bool t), None), Nil))) t[@@ocaml.warning
                                                           "-39"]
and executeCommandOptions_to_json t =
  (fun { commands } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("commands", commands,
               ((fun t -> Json.of_list t) (fun t -> Json.of_string t)), None),
              Nil))) t[@@ocaml.warning "-39"]
and textDocumentSyncOptions_to_json t =
  (fun { openClose; change; willSave; willSaveWaitUntil; didSave } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("openClose", openClose, (fun t -> Json.of_bool t), None),
              (Cons
                 (("change", change, textDocumentSyncKind_to_json, None),
                   (Cons
                      (("willSave", willSave, (fun t -> Json.of_bool t),
                         None),
                        (Cons
                           (("willSaveWaitUntil", willSaveWaitUntil,
                              (fun t -> Json.of_bool t), None),
                             (Cons
                                (("didSave", didSave,
                                   ((fun t -> Json.of_option t)
                                      saveOptions_to_json), (Some None)),
                                  Nil))))))))))) t[@@ocaml.warning "-39"]
and saveOptions_to_json t =
  (fun { includeText } ->
     (fun t -> Json.of_record t)
       (let open! Protocol_conv.Runtime.Record_in in
          Cons
            (("includeText", includeText, (fun t -> Json.of_bool t), None),
              Nil))) t[@@ocaml.warning "-39"]
let _ = params_to_json
and _ = result_to_json
and _ = errorData_to_json
and _ = server_capabilities_to_json
and _ = completionOptions_to_json
and _ = codeLensOptions_to_json
and _ = documentOnTypeFormattingOptions_to_json
and _ = documentLinkOptions_to_json
and _ = executeCommandOptions_to_json
and _ = textDocumentSyncOptions_to_json
and _ = saveOptions_to_json
let rec params_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("processId",
            ((fun t -> Json.to_option t) (fun t -> Json.to_int t)),
            (Some None)),
           (Cons
              (("rootPath",
                 ((fun t -> Json.to_option t) (fun t -> Json.to_string t)),
                 (Some None)),
                (Cons
                   (("rootUri",
                      ((fun t -> Json.to_option t) documentUri_of_json),
                      (Some None)),
                     (Cons
                        (("capabilities", client_capabilities_of_json, None),
                          (Cons (("trace", trace_of_json, (Some Off)), Nil))))))))) in
   let constructor processId rootPath rootUri client_capabilities trace =
     { processId; rootPath; rootUri; client_capabilities; trace } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and result_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons (("capabilities", server_capabilities_of_json, None), Nil) in
   let constructor server_capabilities = { server_capabilities } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and errorData_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons (("retry", (fun t -> Json.to_bool t), None), Nil) in
   let constructor retry = { retry } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and server_capabilities_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("textDocumentSync", textDocumentSyncOptions_of_json, None),
           (Cons
              (("hoverProvider", (fun t -> Json.to_bool t), None),
                (Cons
                   (("completionProvider",
                      ((fun t -> Json.to_option t) completionOptions_of_json),
                      (Some None)),
                     (Cons
                        (("definitionProvider", (fun t -> Json.to_bool t),
                           None),
                          (Cons
                             (("typeDefinitionProvider",
                                (fun t -> Json.to_bool t), None),
                               (Cons
                                  (("referencesProvider",
                                     (fun t -> Json.to_bool t), None),
                                    (Cons
                                       (("documentHighlightProvider",
                                          (fun t -> Json.to_bool t), None),
                                         (Cons
                                            (("documentSymbolProvider",
                                               (fun t -> Json.to_bool t),
                                               None),
                                              (Cons
                                                 (("workspaceSymbolProvider",
                                                    (fun t -> Json.to_bool t),
                                                    None),
                                                   (Cons
                                                      (("codeActionProvider",
                                                         (fun t ->
                                                            Json.to_bool t),
                                                         None),
                                                        (Cons
                                                           (("codeLensProvider",
                                                              ((fun t ->
                                                                  Json.to_option
                                                                    t)
                                                                 codeLensOptions_of_json),
                                                              (Some None)),
                                                             (Cons
                                                                (("documentFormattingProvider",
                                                                   (fun t ->
                                                                    Json.to_bool
                                                                    t), None),
                                                                  (Cons
                                                                    (("documentRangeFormattingProvider",
                                                                    (fun t ->
                                                                    Json.to_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("documentOnTypeFormattingProvider",
                                                                    ((fun t
                                                                    ->
                                                                    Json.to_option
                                                                    t)
                                                                    documentOnTypeFormattingOptions_of_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("renameProvider",
                                                                    (fun t ->
                                                                    Json.to_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("documentLinkProvider",
                                                                    ((fun t
                                                                    ->
                                                                    Json.to_option
                                                                    t)
                                                                    documentLinkOptions_of_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("executeCommandProvider",
                                                                    ((fun t
                                                                    ->
                                                                    Json.to_option
                                                                    t)
                                                                    executeCommandOptions_of_json),
                                                                    (Some
                                                                    None)),
                                                                    (Cons
                                                                    (("typeCoverageProvider",
                                                                    (fun t ->
                                                                    Json.to_bool
                                                                    t), None),
                                                                    (Cons
                                                                    (("rageProvider",
                                                                    (fun t ->
                                                                    Json.to_bool
                                                                    t), None),
                                                                    Nil))))))))))))))))))))))))))))))))))))) in
   let constructor textDocumentSync hoverProvider completionProvider
     definitionProvider typeDefinitionProvider referencesProvider
     documentHighlightProvider documentSymbolProvider workspaceSymbolProvider
     codeActionProvider codeLensProvider documentFormattingProvider
     documentRangeFormattingProvider documentOnTypeFormattingProvider
     renameProvider documentLinkProvider executeCommandProvider
     typeCoverageProvider rageProvider =
     {
       textDocumentSync;
       hoverProvider;
       completionProvider;
       definitionProvider;
       typeDefinitionProvider;
       referencesProvider;
       documentHighlightProvider;
       documentSymbolProvider;
       workspaceSymbolProvider;
       codeActionProvider;
       codeLensProvider;
       documentFormattingProvider;
       documentRangeFormattingProvider;
       documentOnTypeFormattingProvider;
       renameProvider;
       documentLinkProvider;
       executeCommandProvider;
       typeCoverageProvider;
       rageProvider
     } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and completionOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("resolveProvider", (fun t -> Json.to_bool t), None),
           (Cons
              (("triggerCharacters",
                 ((fun t -> Json.to_list t) (fun t -> Json.to_string t)),
                 None), Nil))) in
   let constructor resolveProvider triggerCharacters =
     { resolveProvider; triggerCharacters } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and codeLensOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons (("resolveProvider", (fun t -> Json.to_bool t), None), Nil) in
   let constructor codelens_resolveProvider = { codelens_resolveProvider } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and documentOnTypeFormattingOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("firstTriggerCharacter", (fun t -> Json.to_string t), None),
           (Cons
              (("moreTriggerCharacter",
                 ((fun t -> Json.to_list t) (fun t -> Json.to_string t)),
                 None), Nil))) in
   let constructor firstTriggerCharacter moreTriggerCharacter =
     { firstTriggerCharacter; moreTriggerCharacter } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and documentLinkOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("doclink_resolveProvider", (fun t -> Json.to_bool t), None), Nil) in
   let constructor doclink_resolveProvider = { doclink_resolveProvider } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and executeCommandOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("commands",
            ((fun t -> Json.to_list t) (fun t -> Json.to_string t)), None),
           Nil) in
   let constructor commands = { commands } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and textDocumentSyncOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons
         (("openClose", (fun t -> Json.to_bool t), None),
           (Cons
              (("change", textDocumentSyncKind_of_json, None),
                (Cons
                   (("willSave", (fun t -> Json.to_bool t), None),
                     (Cons
                        (("willSaveWaitUntil", (fun t -> Json.to_bool t),
                           None),
                          (Cons
                             (("didSave",
                                ((fun t -> Json.to_option t)
                                   saveOptions_of_json), (Some None)), Nil))))))))) in
   let constructor openClose change willSave willSaveWaitUntil didSave =
     { openClose; change; willSave; willSaveWaitUntil; didSave } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
and saveOptions_of_json t =
  (let of_funcs =
     let open Protocol_conv.Runtime.Record_in in
       Cons (("includeText", (fun t -> Json.to_bool t), None), Nil) in
   let constructor includeText = { includeText } in
   (fun t -> Json.to_record t) of_funcs constructor) t[@@ocaml.warning "-39"]
let _ = params_of_json
and _ = result_of_json
and _ = errorData_of_json
and _ = server_capabilities_of_json
and _ = completionOptions_of_json
and _ = codeLensOptions_of_json
and _ = documentOnTypeFormattingOptions_of_json
and _ = documentLinkOptions_of_json
and _ = executeCommandOptions_of_json
and _ = textDocumentSyncOptions_of_json
and _ = saveOptions_of_json
          [@@@end]
let params_of_json t =
  let {Logger. log} = Logger.for_section "lsp" in
  log ~title:"debug" "so much fun";
  try
    let p = params_of_json t in
    log ~title:"debug" "so much fun again";
    p
  with exn ->
    log ~title:"debug" "wtf here %s" (Printexc.to_string exn);
    raise exn

end

(* Goto Definition request, method="textDocument/definition" *)
module Definition = struct
  type params = TextDocumentPositionParams.t [@@deriving protocol ~driver:(module Json)]

  and result = DefinitionLocation.t list  (* wire: either a single one or an array *)
end

(* Goto Type Definition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct
  type params = TextDocumentPositionParams.t [@@deriving protocol ~driver:(module Json)]

  and result = Location.t list  (* wire: either a single one or an array *)
end

(* References request, method="textDocument/references" *)
module References = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: referenceContext;
  } [@@deriving protocol ~driver:(module Json)]

  and referenceContext = {
    includeDeclaration: bool;
  }

  and result = Location.t list (* wire: either a single one or an array *)
end

(* DocumentHighlight request, method="textDocument/documentHighlight" *)
module TextDocumentHighlight = struct
  type params = TextDocumentPositionParams.t [@@deriving protocol ~driver:(module Json)]

  and result = DocumentHighlight.t list (* wire: either a single one or an array *)
end

module SymbolKind = struct

  type t =
    | File  (* 1 *)
    | Module  (* 2 *)
    | Namespace  (* 3 *)
    | Package  (* 4 *)
    | Class  (* 5 *)
    | Method  (* 6 *)
    | Property  (* 7 *)
    | Field  (* 8 *)
    | Constructor  (* 9 *)
    | Enum  (* 10 *)
    | Interface  (* 11 *)
    | Function  (* 12 *)
    | Variable  (* 13 *)
    | Constant  (* 14 *)
    | String  (* 15 *)
    | Number  (* 16 *)
    | Boolean  (* 17 *)
    | Array  (* 18 *)
    | Object (* 19 *)
    | Key (* 20 *)
    | Null (* 21 *)
    | EnumMember (* 22 *)
    | Struct (* 23 *)
    | Event (* 24 *)
    | Operator (* 25 *)
    | TypeParameter (* 26 *)

  let to_json = function
    | File -> `Int 1
    | Module -> `Int 2
    | Namespace -> `Int 3
    | Package  -> `Int 4
    | Class -> `Int 5
    | Method -> `Int 6
    | Property -> `Int 7
    | Field -> `Int 8
    | Constructor -> `Int 9
    | Enum -> `Int 10
    | Interface -> `Int 11
    | Function -> `Int 12
    | Variable -> `Int 13
    | Constant -> `Int 14
    | String -> `Int 15
    | Number -> `Int 16
    | Boolean -> `Int 17
    | Array -> `Int 18
    | Object -> `Int 19
    | Key -> `Int 20
    | Null -> `Int 21
    | EnumMember -> `Int 22
    | Struct -> `Int 23
    | Event -> `Int 24
    | Operator -> `Int 25
    | TypeParameter -> `Int 26

  let of_json = function
    | `Int 1 -> File
    | `Int 2 -> Module
    | `Int 3 -> Namespace
    | `Int 4 -> Package
    | `Int 5 -> Class
    | `Int 6 -> Method
    | `Int 7 -> Property
    | `Int 8 -> Field
    | `Int 9 -> Constructor
    | `Int 10 -> Enum
    | `Int 11 -> Interface
    | `Int 12 -> Function
    | `Int 13 -> Variable
    | `Int 14 -> Constant
    | `Int 15 -> String
    | `Int 16 -> Number
    | `Int 17 -> Boolean
    | `Int 18 -> Array
    | `Int 19 -> Object
    | `Int 20 -> Key
    | `Int 21 -> Null
    | `Int 22 -> EnumMember
    | `Int 23 -> Struct
    | `Int 24 -> Event
    | `Int 25 -> Operator
    | `Int 26 -> TypeParameter
    | _ -> invalid_arg "invalid SymbolKind"

end

module SymbolInformation = struct
 type t = {
    name : string;
    kind : SymbolKind.t;
    deprecated : bool [@default false];
    (* the span of the symbol including its contents *)
    location : Location.t;
    (* the symbol containing this symbol *)
    containerName : string option [@default None];
  } [@@deriving protocol ~driver:(module Json)]
end

module DocumentSymbol = struct

  type t = {
    (**
     * The name of this symbol. Will be displayed in the user interface and
     * therefore must not be an empty string or a string only consisting of
     * white spaces.
     *)
    name : string;

    (**
     * More detail for this symbol, e.g the signature of a function.
     *)
    detail: string option;

    (**
     * The kind of this symbol.
     *)
    kind: SymbolKind.t;

    (**
     * Indicates if this symbol is deprecated.
     *)
    deprecated : bool;

    (**
     * The range enclosing this symbol not including leading/trailing whitespace
     * but everything else like comments. This information is typically used to
     * determine if the clients cursor is inside the symbol to reveal in the
     * symbol in the UI.
     *)
    range : range;

    (**
     * The range that should be selected and revealed when this symbol is being
     * picked, e.g the name of a function.  Must be contained by the `range`.
     *)
    selectionRange : range;

    (**
     * Children of this symbol, e.g. properties of a class.
     *)
    children: t list;
  } [@@deriving protocol ~driver:(module Json)]
end

(* Document Symbols request, method="textDocument/documentSymbols" *)
module TextDocumentDocumentSymbol = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@deriving protocol ~driver:(module Json)]

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  let result_to_json = function
    | DocumentSymbol symbols ->
      `List (Std.List.map symbols ~f:DocumentSymbol.to_json)
    | SymbolInformation symbols ->
      `List (Std.List.map symbols ~f:SymbolInformation.to_json)

end

module CodeLens = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@deriving protocol ~driver:(module Json)]

  and result = item list

  and item = {
    range: range;
    command: Command.t option;
  }
end

(** Rename symbol request, metho="textDocument/rename" *)
module Rename = struct
 type params = {
   textDocument: TextDocumentIdentifier.t; (** The document to rename. *)
   position: position; (** The position at which this request was sent. *)
   newName: string; (** The new name of the symbol. If the given name
                        is not valid the request must return a
                        [ResponseError](#ResponseError) with an
                        appropriate message set. *)
  } [@@deriving protocol ~driver:(module Json)]

  type result = WorkspaceEdit.t [@@deriving to_protocol ~driver:(module Json)]
end

module DebugEcho = struct
  type params = {
    message: string;
  } [@@deriving protocol ~driver:(module Json)]

  and result = params
end

module DebugTextDocumentGet = struct
  type params = TextDocumentPositionParams.t [@@deriving protocol ~driver:(module Json)]

  type result = string option [@default None]

  let result_to_json = function
    | Some s -> `String s
    | None -> `Null
end
