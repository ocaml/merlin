type t = {
  ic : in_channel;
  oc : out_channel;
  fd : Unix.file_descr;
  mutable state : state;
}

and state =
  | Ready
  | Initialized of Protocol.Initialize.client_capabilities
  | Closed

let {Logger. log} = Logger.for_section "lsp"

let send rpc json =
  log ~title:"debug" "send: %a" (fun () -> Yojson.Safe.pretty_to_string ~std:false) json;
  let data = Yojson.Safe.to_string json in
  let length = String.length data in
  let contentLengthString =
    "Content-Length: " ^ string_of_int length ^ "\r\n"
  in
  output_string rpc.oc contentLengthString;
  output_string rpc.oc "\r\n";
  output_string rpc.oc data;
  flush rpc.oc

module Headers = struct
  type t = {
    content_length: int option;
  }

  let initial = {
    content_length = None;
  }

  let content_length = "Content-Length: "
  let content_length_len = String.length content_length

  let end_line = "\r\n"
  let end_line_len = String.length end_line

  let has_content_length s =
    String.length s > content_length_len &&
    String.equal (String.sub s 0 content_length_len) content_length

  let parse_content_length line =
    let v =
      String.sub line
        content_length_len
        (String.length line - end_line_len - content_length_len)
    in
    int_of_string v

  type state =
    | Partial of t
    | Done of t

  let parse_line headers line =
    if String.equal line "\r\n"
    then Done headers
    else
      if has_content_length line
      then
        let content_length = parse_content_length line in
        Partial {content_length = Some content_length;}
      else Partial headers

  let read ic =
    let rec loop headers =
      let line = input_line ic in
      match parse_line headers (line ^ "\n") with
      | Partial headers -> loop headers
      | Done headers -> headers
    in
    loop initial
end

module Packet = struct
  type t = {
    id: int option [@default None];
    method_: string [@key "method"];
    params: Yojson.Safe.json;
  } [@@deriving yojson { strict = false }]
end

let read rpc =
  let open Utils.Result.Infix in

  let read_content rpc =
    Thread.wait_read rpc.fd;
    let headers = Headers.read rpc.ic in
    match headers.content_length with
    | Some len ->
      let buffer = Bytes.create len in
      let rec read_loop read =
        if read < len
        then
          let n = input rpc.ic buffer read (len - read) in
          read_loop (read + n)
        else ()
      in
      let () = read_loop 0 in
      Ok (Bytes.to_string buffer)
    | None ->
      Error "missing Content-length header"
  in

  let parse_json content =
    match Yojson.Safe.from_string content with
    | json ->
      log ~title:"debug" "recv: %a" (fun () -> Yojson.Safe.pretty_to_string ~std:false) json;
      Ok json
    | exception Yojson.Json_error msg ->
      errorf "error parsing json: %s" msg
  in

  read_content rpc >>= parse_json >>= Packet.of_yojson

module Response = struct
  type response = {
    id : int;
    jsonrpc: string;
    result : Yojson.Safe.json;
  } [@@deriving yojson]

  type response_error = {
    id : int;
    jsonrpc: string;
    error : error;
  } [@@deriving yojson]

  and error = {
    code : int;
    message : string;
  }

  type t =
    | Response of response
    | Response_error of response_error

  let make id result =
    Response {id; result; jsonrpc="2.0"}

  let make_error id code message =
    Response_error {id; error = {code; message;}; jsonrpc="2.0"}

  let to_yojson = function
    | Response v -> response_to_yojson v
    | Response_error v -> response_error_to_yojson v
end

let send_response rpc (response : Response.t) =
  let json = Response.to_yojson response in
  send rpc json

module Server_notification = struct
  open Protocol 

  type t =
    | PublishDiagnostics of PublishDiagnostics.params

  let method_ = function
    | PublishDiagnostics _ -> "textDocument/publishDiagnostics"

  let params_to_yojson = function
    | PublishDiagnostics params -> PublishDiagnostics.params_to_yojson params

end

let send_notification rpc notif =
  let method_ = Server_notification.method_ notif in
  let params = Server_notification.params_to_yojson notif in
  let response = `Assoc [("jsonrpc", (`String "2.0")); ("method", (`String method_)); ("params", params)] in
  send rpc response

module Client_notification = struct
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.json
end

module Request = struct
  open Protocol

  type _ t =
    | Shutdown : unit t
    | TextDocumentHover : Hover.params -> Hover.result t
    | TextDocumentDefinition : Definition.params -> Definition.result t
    | TextDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result t
    | TextDocumentCompletion : Completion.params -> Completion.result t
    | TextDocumentCodeLens : CodeLens.params -> CodeLens.result t
    | TextDocumentRename : Rename.params -> Rename.result t
    | DocumentSymbol : TextDocumentDocumentSymbol.params -> TextDocumentDocumentSymbol.result t
    | DebugEcho : DebugEcho.params -> DebugEcho.result t
    | DebugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result t
    | TextDocumentReferences : References.params -> References.result t
    | TextDocumentHighlight : TextDocumentHighlight.params -> TextDocumentHighlight.result t
    | UnknownRequest : string * Yojson.Safe.json -> unit t

  let request_result_to_response (type a) id (req : a t) (result : a) =
    match req, result with
    | Shutdown, _resp -> None
    | TextDocumentHover _, result ->
      let json = Hover.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentDefinition _, result ->
      let json = Definition.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentTypeDefinition _, result ->
      let json = TypeDefinition.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentCompletion _, result ->
      let json = Completion.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentCodeLens _, result ->
      let json = CodeLens.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentRename _, result ->
      let json = Rename.result_to_yojson result in
      Some (Response.make id json)
    | DocumentSymbol _, result ->
      let json = TextDocumentDocumentSymbol.result_to_yojson result in
      Some (Response.make id json)
    | DebugEcho _, result ->
      let json = DebugEcho.result_to_yojson result in
      Some (Response.make id json)
    | DebugTextDocumentGet _, result ->
      let json = DebugTextDocumentGet.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentReferences _, result ->
      let json = References.result_to_yojson result in
      Some (Response.make id json)
    | TextDocumentHighlight _, result ->
      let json = TextDocumentHighlight.result_to_yojson result in
      Some (Response.make id json)
    | UnknownRequest _, _resp -> None
end

module Message = struct
  open Protocol

  type t =
    | Initialize : int * Protocol.Initialize.params -> t
    | Request : int * 'result Request.t -> t
    | Client_notification : Client_notification.t -> t

  let parse packet =
    let open Utils.Result.Infix in
    match packet.Packet.id with
    | Some id ->
      begin match packet.method_ with
      | "initialize" ->
        Protocol.Initialize.params_of_yojson packet.params >>= fun params ->
        Ok (Initialize (id, params))
      | "shutdown" ->
        Ok (Request (id, Shutdown))
      | "textDocument/completion" ->
        Completion.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentCompletion params))
      | "textDocument/documentSymbol" ->
        TextDocumentDocumentSymbol.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DocumentSymbol params))
      | "textDocument/hover" ->
        Hover.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentHover params))
      | "textDocument/definition" ->
        Definition.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentDefinition params))
      | "textDocument/typeDefinition" ->
        TypeDefinition.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentTypeDefinition params))
      | "textDocument/references" ->
        References.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentReferences params))
      | "textDocument/codeLens" ->
        CodeLens.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentCodeLens params))
      | "textDocument/rename" ->
        Rename.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentRename params))
      | "textDocument/documentHighlight" ->
        TextDocumentHighlight.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, TextDocumentHighlight params))
      | "debug/echo" ->
        DebugEcho.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DebugEcho params))
      | "debug/textDocument/get" ->
        DebugTextDocumentGet.params_of_yojson packet.params >>= fun params ->
        Ok (Request (id, DebugTextDocumentGet params))
      | name ->
        Ok (Request (id, UnknownRequest (name, packet.params)))
      end
    | None ->
      begin match packet.method_ with
      | "textDocument/didOpen" ->
        DidOpen.params_of_yojson packet.params >>= fun params ->
        Ok (Client_notification (TextDocumentDidOpen params))
      | "textDocument/didChange" ->
        DidChange.params_of_yojson packet.params >>= fun params ->
        Ok (Client_notification (TextDocumentDidChange params))
      | "exit" ->
        Ok (Client_notification Exit)
      | "initialized" ->
        Ok (Client_notification Initialized)
      | _ ->
        Ok (Client_notification (UnknownNotification (packet.method_, packet.params)))
      end

end

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

let start init_state handler ic oc =
  let open Utils.Result.Infix in

  let read_message rpc =
    read rpc >>= fun packet ->
    Message.parse packet
  in

  let handle_message prev_state f =
    let start = Unix.gettimeofday () in
    let next_state = f () in
    let ellapsed = (Unix.gettimeofday () -. start) /. 1000.0 in
    log ~title:"debug" "time elapsed processing message: %fs" ellapsed;
    match next_state with
    | Ok next_state -> next_state
    | Error msg ->
      log ~title:"error" "%s" msg;
      prev_state
  in

  let rec loop rpc state =
    match rpc.state with
    | Closed -> ()
    | Ready ->
      let next_state =
        handle_message state (fun () ->
          read_message rpc >>= function
          | Message.Initialize (id, params) ->
            handler.on_initialize rpc state params >>= fun (next_state, result) ->
            let json = Protocol.Initialize.result_to_yojson result in
            let response = Response.make id json in
            rpc.state <- Initialized params.client_capabilities;
            send_response rpc response;
            Ok next_state

          | Message.Client_notification Exit ->
            rpc.state <- Closed;
            Ok state
          | Message.Client_notification _ ->
            (* we drop all notifications per protocol before we initialized *)
            Ok state

          | Message.Request (id, _) ->
            (* we response with -32002 per protocol before we initialized  *)
            let response = Response.make_error id (-32002) "not initialized" in
            send_response rpc response;
            Ok state
        )
      in
      Logger.log_flush ();
      loop rpc next_state
    | Initialized client_capabilities ->
      let next_state =
        handle_message state (fun () ->
          read_message rpc >>= function
          | Message.Initialize _ ->
            errorf "received another initialize request"
          | Message.Client_notification (Exit as notif) ->
            rpc.state <- Closed;
            handler.on_notification rpc state notif
          | Message.Client_notification notif ->
            handler.on_notification rpc state notif
          | Message.Request (id, req) ->
            handler.on_request rpc state client_capabilities req >>= fun (next_state, result) ->
            begin match Request.request_result_to_response id req result with
            | None ->
              Ok next_state
            | Some response ->
              send_response rpc response;
              Ok next_state
            end
        )
      in
      Logger.log_flush ();
      loop rpc next_state
  in

  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  let fd = Unix.descr_of_in_channel stdin in
  let rpc = { ic; oc; fd; state = Ready; } in
  loop rpc init_state

let stop (rpc : t) = rpc.state <- Closed
