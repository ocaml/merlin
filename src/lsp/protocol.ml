(**
 * This encodes LSP protocol specification as document at
 *
 *   https://microsoft.github.io/language-server-protocol/specification
 *
 * Most of this was borrowed from facebook/flow repository.
 *
 *)

type documentUri = Uri.t [@@deriving yojson]

type zero_based_int = int [@@deriving yojson]

type position = {
  line: zero_based_int;
  character: zero_based_int;
} [@@deriving yojson { strict = false }]

type range = {
  start_: position [@key "start"];
  end_: position [@key "end"];
} [@@deriving yojson { strict = false }]

module Command = struct
  type t = {
    title : string;
    command : string;
  } [@@deriving yojson]
end

module MarkupKind = struct
  type t =
    | Plaintext
    | Markdown

  let to_yojson = function
    | Plaintext -> `String "plaintext"
    | Markdown -> `String "markdown"

  let of_yojson = function
    | `String "plaintext" -> Ok Plaintext
    | `String "markdown" -> Ok Markdown
    | `String _ -> Ok Plaintext
    | _ -> Error "invalid contentFormat"
end

module MarkupContent = struct
  type t = {
    value: string;
    kind: MarkupKind.t;
  } [@@deriving yojson { strict = false }]
end

module Location = struct
  type t = {
    uri: Uri.t;
    range : range;
  } [@@deriving yojson { strict = false }]
end

module DefinitionLocation = struct
  type t = {
    uri: Uri.t;
    range : range;
    title: string option [@default None];
  } [@@deriving yojson { strict = false }]
end

(* Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
  } [@@deriving yojson { strict = false }]
end

(* An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    version: int;  (* the version number of this document *)
  } [@@deriving yojson { strict = false }]
end

(* An item to transfer a text document from the client to the server. The
   version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    languageId: string;  (* the text document's language identifier *)
    version: int;  (* the version of the document *)
    text: string;  (* the content of the opened text document *)
  } [@@deriving yojson { strict = false }]
end

(* DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams [@@deriving yojson { strict = false }]

  and didOpenTextDocumentParams = {
    textDocument: TextDocumentItem.t;  (* the document that was opened *)
  }
end

(* DidChangeTextDocument notification, method="textDocument/didChange" *)
module DidChange = struct
  type params = didChangeTextDocumentParams [@@deriving yojson { strict = true }]

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
  } [@@deriving yojson { strict = false }]
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

  let kind_to_yojson = function
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let kind_of_yojson = function
    | `Int 1 -> Ok Text
    | `Int 2 -> Ok Read
    | `Int 3 -> Ok Write
    | _ -> Error "expected int between 1 and 3"

  type t = {
    range: range;
    kind: kind option;
  } [@@deriving yojson { strict = false }]

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
  } [@@deriving yojson { strict = false }]
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
  } [@@deriving yojson { strict = false }]
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

  let changes_to_yojson changes =
    let changes =
      List.map (fun (uri, edits) ->
        let uri = Uri.to_string uri in
        let edits = `List (List.map TextEdit.to_yojson edits) in
        uri, edits
      ) changes
    in
    `Assoc changes

  type documentChanges = TextDocumentEdit.t list [@@deriving to_yojson]

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
  } [@@deriving to_yojson { strict = false }]

  let empty = {
    changes = None;
    documentChanges = None;
  }

  (** Create a {!type:t} based on the capabilities of the client. *)
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

  let diagnosticCode_to_yojson = function
    | IntCode v -> `Int v
    | StringCode v -> `String v
    | NoCode -> `Null

  let diagnosticCode_of_yojson = function
    | `Int v -> Ok (IntCode v)
    | `String v -> Ok (StringCode v)
    | `Null -> Ok NoCode
    | _ -> Error "invalid diagnostic.code"

  type diagnosticSeverity =
    | Error (* 1 *)
    | Warning (* 2 *)
    | Information (* 3 *)
    | Hint (* 4 *)

  let diagnosticSeverity_to_yojson = function
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let diagnosticSeverity_of_yojson = function
    | `Int 1 -> Ok Error
    | `Int 2 -> Ok Warning
    | `Int 3 -> Ok Information
    | `Int 4 -> Ok Hint
    | _ -> Error "expected int"

  type params = publishDiagnosticsParams [@@deriving yojson { strict = false }]

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

  let completionTriggerKind_to_yojson = function
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let completionTriggerKind_of_yojson = function
    | `Int 1 -> Ok Invoked
    | `Int 2 -> Ok TriggerCharacter
    | `Int 3 -> Ok TriggerForIncompleteCompletions
    | _ -> Error "invalid completion.triggerKind"

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

  let completionItemKind_to_yojson v =
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

  let completionItemKind_of_yojson = function
    | `Int v ->
      begin match completionItemKind_of_int_opt v with
      | Some v -> Ok v
      | None -> Error "invalid completion.kind"
      end
    | _ -> Error "invalid completion.kind: expected an integer"

    (** Keep this in sync with `int_of_completionItemKind`. *)
  type insertTextFormat =
    | PlainText (* 1 *)  (* the insertText/textEdits are just plain strings *)
    | SnippetFormat (* 2 *)  (* wire: just "Snippet" *)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with insertFormat_of_int_opt. *)
  let int_of_insertFormat = function
    | PlainText -> 1
    | SnippetFormat -> 2

  let insertTextFormat_to_yojson v =
    `Int (int_of_insertFormat v)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with int_of_insertFormat. *)
  let insertFormat_of_int_opt = function
    | 1 -> Some PlainText
    | 2 -> Some SnippetFormat
    | _ -> None

  let insertTextFormat_of_yojson = function
    | `Int v ->
      begin match insertFormat_of_int_opt v with
      | Some v -> Ok v
      | None -> Error "invalid completion.kind"
      end
    | _ -> Error "invalid completion.kind: expected an integer"

  type params = completionParams [@@deriving yojson { strict = false }]

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
    textEdit: TextEdit.t option [@default None];
    additionalTextEdits: TextEdit.t list [@default []];
    (* command: Command.t option [@default None];  (1* if present, is executed after completion *1) *)
    (* data: Hh_json.json option [@default None]; *)
  }
end

(* Hover request, method="textDocument/hover" *)
module Hover = struct
  type params =
    TextDocumentPositionParams.t
    [@@deriving yojson { strict = false }]

  and result = hoverResult option [@default None]

  and hoverResult = {
    contents: MarkupContent.t;
    range: range option [@default None];
  }
end

(* Initialize request, method="initialize" *)
module Initialize = struct

  type trace =
    | Off
    | Messages
    | Verbose

  let trace_to_yojson = function
    | Off -> `String "off"
    | Messages -> `String "messages"
    | Verbose -> `String "verbose"

  let trace_of_yojson = function
    | `String "off" -> Ok Off
    | `String "messages" -> Ok Messages
    | `String "verbose" -> Ok Verbose
    | _ -> Error "invalid trace"

  type textDocumentSyncKind =
    | NoSync (* 0 *)  (* docs should not be synced at all. Wire "None" *)
    | FullSync (* 1 *)  (* synced by always sending full content. Wire "Full" *)
    | IncrementalSync (* 2 *)  (* full only on open. Wire "Incremental" *)

  let textDocumentSyncKind_to_yojson = function
    | NoSync -> `Int 0
    | FullSync -> `Int 1
    | IncrementalSync -> `Int 2

  let textDocumentSyncKind_of_yojson = function
    | `Int 0 -> Ok NoSync
    | `Int 1 -> Ok FullSync
    | `Int 2 -> Ok IncrementalSync
    | _ -> Error "invalid textDocumentSyncKind"

  (* synchronization capabilities say what messages the client is capable
   * of sending, should be be so asked by the server.
   * We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)

  type synchronization = {
    willSave: bool [@default false];  (* client can send textDocument/willSave *)
    willSaveWaitUntil: bool [@default false];  (* textDoc.../willSaveWaitUntil *)
    didSave: bool [@default false];  (* textDocument/didSave *)
  } [@@deriving yojson { strict = false }]

  let synchronization_empty = {
    willSave = true;
    willSaveWaitUntil = true;
    didSave = true;
  }

  type completionItem = {
    snippetSupport: bool [@default false];  (* client can do snippets as insert text *)
  } [@@deriving yojson { strict = false }]

  let completionItem_empty = {
    snippetSupport = false;
  }

  type completion = {
    completionItem: completionItem [@default completionItem_empty];
  } [@@deriving yojson { strict = false }]

  let completion_empty = {
    completionItem = completionItem_empty;
  }

  type hover = {
    contentFormat: MarkupKind.t list [@default [Plaintext]];
  } [@@deriving yojson { strict = false }]

  let hover_empty = {
    contentFormat = [Plaintext];
  }

  type documentSymbol = {
    hierarchicalDocumentSymbolSupport : bool [@default false];
  } [@@deriving yojson { strict = false }]

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
  } [@@deriving yojson { strict = false }]

  let textDocumentClientCapabilities_empty = {
    completion = completion_empty;
    synchronization = synchronization_empty;
    hover = hover_empty;
    documentSymbol = documentSymbol_empty;
  }

  type workspaceEdit = {
    (** client supports versioned doc changes *)
    documentChanges: bool [@default false];
  } [@@deriving yojson { strict = false }]

  let workspaceEdit_empty = {
    documentChanges = false;
  }

  type workspaceClientCapabilities = {
    (** client supports applying batch edits *)
    applyEdit: bool [@default false];
    workspaceEdit: workspaceEdit [@default workspaceEdit_empty];
    (** omitted: dynamic-registration fields *)
  } [@@deriving yojson { strict = false }]

  let workspaceClientCapabilities_empty = {
    applyEdit = false;
    workspaceEdit = workspaceEdit_empty;
  }

  type windowClientCapabilities = {
    (* Nuclide-specific: client supports window/showStatusRequest *)
    status: bool [@default false];
    (* Nuclide-specific: client supports window/progress *)
    progress: bool [@default false];
    (* Nuclide-specific: client supports window/actionRequired *)
    actionRequired: bool [@default false];
  } [@@deriving yojson { strict = false }]

  let windowClientCapabilities_empty = {
    status = true;
    progress = true;
    actionRequired = true;
  }

  type telemetryClientCapabilities = {
    (* Nuclide-specific: client supports telemetry/connectionStatus *)
    connectionStatus: bool [@default false];
  } [@@deriving yojson { strict = false }]

  let telemetryClientCapabilities_empty = {
    connectionStatus = true;
  }

  type client_capabilities = {
    workspace: workspaceClientCapabilities [@default workspaceClientCapabilities_empty];
    textDocument: textDocumentClientCapabilities [@default textDocumentClientCapabilities_empty];
    window: windowClientCapabilities [@default windowClientCapabilities_empty];
    telemetry: telemetryClientCapabilities [@default telemetryClientCapabilities_empty];
    (* omitted: experimental *)
  } [@@deriving yojson { strict = false }]

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
    client_capabilities: client_capabilities [@key "capabilities"] [@default client_capabilities_empty];
    trace: trace [@default Off];  (* the initial trace setting, default="off" *)
  } [@@deriving yojson { strict = false }]

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
end

(* Goto Definition request, method="textDocument/definition" *)
module Definition = struct
  type params = TextDocumentPositionParams.t [@@deriving yojson { strict = false }]

  and result = DefinitionLocation.t list  (* wire: either a single one or an array *)
end

(* Goto Type Definition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct
  type params = TextDocumentPositionParams.t [@@deriving yojson { strict = false }]

  and result = Location.t list  (* wire: either a single one or an array *)
end

(* References request, method="textDocument/references" *)
module References = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: referenceContext;
  } [@@deriving yojson { strict = false }]

  and referenceContext = {
    includeDeclaration: bool;
  }

  and result = Location.t list (* wire: either a single one or an array *)
end

(* DocumentHighlight request, method="textDocument/documentHighlight" *)
module TextDocumentHighlight = struct
  type params = TextDocumentPositionParams.t [@@deriving yojson { strict = false }]

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

  let to_yojson = function
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

  let of_yojson = function
    | `Int 1 -> Ok File
    | `Int 2 -> Ok Module
    | `Int 3 -> Ok Namespace
    | `Int 4 -> Ok Package
    | `Int 5 -> Ok Class
    | `Int 6 -> Ok Method
    | `Int 7 -> Ok Property
    | `Int 8 -> Ok Field
    | `Int 9 -> Ok Constructor
    | `Int 10 -> Ok Enum
    | `Int 11 -> Ok Interface
    | `Int 12 -> Ok Function
    | `Int 13 -> Ok Variable
    | `Int 14 -> Ok Constant
    | `Int 15 -> Ok String
    | `Int 16 -> Ok Number
    | `Int 17 -> Ok Boolean
    | `Int 18 -> Ok Array
    | `Int 19 -> Ok Object
    | `Int 20 -> Ok Key
    | `Int 21 -> Ok Null
    | `Int 22 -> Ok EnumMember
    | `Int 23 -> Ok Struct
    | `Int 24 -> Ok Event
    | `Int 25 -> Ok Operator
    | `Int 26 -> Ok TypeParameter
    | _ -> Error "invalid SymbolKind"

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
  } [@@deriving yojson]
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
  } [@@deriving yojson]
end

(* Document Symbols request, method="textDocument/documentSymbols" *)
module TextDocumentDocumentSymbol = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@deriving yojson]

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  let result_to_yojson = function
    | DocumentSymbol symbols ->
      `List (Std.List.map symbols ~f:DocumentSymbol.to_yojson)
    | SymbolInformation symbols ->
      `List (Std.List.map symbols ~f:SymbolInformation.to_yojson)

end

module CodeLens = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@deriving yojson]

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
  } [@@deriving yojson]

  type result = WorkspaceEdit.t [@@deriving to_yojson]
end

module DebugEcho = struct
  type params = {
    message: string;
  } [@@deriving yojson { strict = false }]

  and result = params
end

module DebugTextDocumentGet = struct
  type params = TextDocumentPositionParams.t [@@deriving yojson { strict = false }]

  type result = string option [@default None]

  let result_to_yojson = function
    | Some s -> `String s
    | None -> `Null
end
