(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

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

(**
 * This encodes LSP RPC state machine.
 *)

module Server_notification : sig
  open Protocol

  type t =
    | PublishDiagnostics of PublishDiagnostics.publishDiagnosticsParams
end

module Client_notification : sig
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.json
end

module Request : sig
  open Protocol

  type _ t =
    | Shutdown : unit t
    | TextDocumentHover : Hover.params -> Hover.result t
    | TextDocumentDefinition : Definition.params -> Definition.result t
    | TextDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result t
    | TextDocumentCompletion : Completion.params -> Completion.result t
    | DocumentSymbol : DocumentSymbol.params -> DocumentSymbol.result t
    | DebugEcho : DebugEcho.params -> DebugEcho.result t
    | DebugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result t
    | UnknownRequest : string * Yojson.Safe.json -> unit t
end

type t

type 'state handler = {
  on_initialize :
    t
    -> 'state
    -> Protocol.Initialize.params
    -> ('state * Protocol.Initialize.result, string) result;

  on_request :
    'res.
    t
    -> 'state
    -> Protocol.Initialize.client_capabilities
    -> 'res Request.t
    -> ('state * 'res, string) result;

  on_notification :
    t
    -> 'state
    -> Client_notification.t
    -> ('state, string) result
}

val start :
  'state
  -> 'state handler
  -> in_channel
  -> out_channel
  -> unit

val stop : t -> unit

val send_notification : t -> Server_notification.t -> unit
