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

type yojson = Yojson.Safe.t
let yojson_of_yojson d = d
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
    String.compare (String.sub s 0 content_length_len) content_length = 0

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
    if String.compare line "\r\n" = 0
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
    params: yojson;
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/rpc.ml.Packet.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let id_field = ref None
       and method__field = ref None
       and params_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "id" ->
                   (match Ppx_yojson_conv_lib.(!) id_field with
                    | None ->
                        let fvalue =
                          option_of_yojson int_of_yojson _field_yojson in
                        id_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "method" ->
                   (match Ppx_yojson_conv_lib.(!) method__field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        method__field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "params" ->
                   (match Ppx_yojson_conv_lib.(!) params_field with
                    | None ->
                        let fvalue = yojson_of_yojson _field_yojson in
                        params_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ -> ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) id_field),
                           (Ppx_yojson_conv_lib.(!) method__field),
                           (Ppx_yojson_conv_lib.(!) params_field))
                   with
                   | (id_value, Some method__value, Some params_value) ->
                       {
                         id =
                           ((match id_value with | None -> None | Some v -> v));
                         method_ = method__value;
                         params = params_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) method__field) None),
                            "method_");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) params_field) None),
                           "params")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { id = v_id; method_ = v_method_; params = v_params } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_yojson v_params in ("params", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_method_ in ("method", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_int v_id in ("id", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
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
      Utils.Result.errorf "error parsing json: %s" msg
  in

  read_content rpc >>= parse_json
  >>= fun parsed -> match Packet.t_of_yojson parsed with
  | r -> Ok r
  | exception _exn -> Error "Unexpected packet"


module Response = struct
  type response = {
    id : int;
    jsonrpc: string;
    result : yojson;
  }
  [@@deriving_inline yojson]
  
let _ = fun (_ : response) -> ()
let response_of_yojson =
  (let _tp_loc = "src/lsp/rpc.ml.Response.response" in
   function
   | `Assoc field_yojsons as yojson ->
       let id_field = ref None
       and jsonrpc_field = ref None
       and result_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "id" ->
                   (match Ppx_yojson_conv_lib.(!) id_field with
                    | None ->
                        let fvalue = int_of_yojson _field_yojson in
                        id_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "jsonrpc" ->
                   (match Ppx_yojson_conv_lib.(!) jsonrpc_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        jsonrpc_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "result" ->
                   (match Ppx_yojson_conv_lib.(!) result_field with
                    | None ->
                        let fvalue = yojson_of_yojson _field_yojson in
                        result_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) id_field),
                           (Ppx_yojson_conv_lib.(!) jsonrpc_field),
                           (Ppx_yojson_conv_lib.(!) result_field))
                   with
                   | (Some id_value, Some jsonrpc_value, Some result_value)
                       ->
                       {
                         id = id_value;
                         jsonrpc = jsonrpc_value;
                         result = result_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) id_field) None), "id");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) jsonrpc_field) None),
                           "jsonrpc");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) result_field) None),
                           "result")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> response)
let _ = response_of_yojson
let yojson_of_response =
  (function
   | { id = v_id; jsonrpc = v_jsonrpc; result = v_result } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_yojson v_result in ("result", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_jsonrpc in ("jsonrpc", arg) :: bnds in
       let bnds = let arg = yojson_of_int v_id in ("id", arg) :: bnds in
       `Assoc bnds : response -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_response
[@@@end]

  type response_error = {
    id : int;
    jsonrpc: string;
    error : error;
  }

  and error = {
    code : int;
    message : string;
  }
  [@@deriving_inline yojson]
  
let _ = fun (_ : response_error) -> ()
let _ = fun (_ : error) -> ()
let rec response_error_of_yojson =
  (let _tp_loc = "src/lsp/rpc.ml.Response.response_error" in
   function
   | `Assoc field_yojsons as yojson ->
       let id_field = ref None
       and jsonrpc_field = ref None
       and error_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "id" ->
                   (match Ppx_yojson_conv_lib.(!) id_field with
                    | None ->
                        let fvalue = int_of_yojson _field_yojson in
                        id_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "jsonrpc" ->
                   (match Ppx_yojson_conv_lib.(!) jsonrpc_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        jsonrpc_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "error" ->
                   (match Ppx_yojson_conv_lib.(!) error_field with
                    | None ->
                        let fvalue = error_of_yojson _field_yojson in
                        error_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) id_field),
                           (Ppx_yojson_conv_lib.(!) jsonrpc_field),
                           (Ppx_yojson_conv_lib.(!) error_field))
                   with
                   | (Some id_value, Some jsonrpc_value, Some error_value) ->
                       {
                         id = id_value;
                         jsonrpc = jsonrpc_value;
                         error = error_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) id_field) None), "id");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) jsonrpc_field) None),
                           "jsonrpc");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) error_field) None),
                           "error")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> response_error)
and error_of_yojson =
  (let _tp_loc = "src/lsp/rpc.ml.Response.error" in
   function
   | `Assoc field_yojsons as yojson ->
       let code_field = ref None
       and message_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "code" ->
                   (match Ppx_yojson_conv_lib.(!) code_field with
                    | None ->
                        let fvalue = int_of_yojson _field_yojson in
                        code_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "message" ->
                   (match Ppx_yojson_conv_lib.(!) message_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        message_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) code_field),
                           (Ppx_yojson_conv_lib.(!) message_field))
                   with
                   | (Some code_value, Some message_value) ->
                       { code = code_value; message = message_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) code_field) None),
                            "code");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) message_field) None),
                           "message")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> error)
let _ = response_error_of_yojson
and _ = error_of_yojson
let rec yojson_of_response_error =
  (function
   | { id = v_id; jsonrpc = v_jsonrpc; error = v_error } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_error v_error in ("error", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_jsonrpc in ("jsonrpc", arg) :: bnds in
       let bnds = let arg = yojson_of_int v_id in ("id", arg) :: bnds in
       `Assoc bnds : response_error -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_error =
  (function
   | { code = v_code; message = v_message } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_message in ("message", arg) :: bnds in
       let bnds = let arg = yojson_of_int v_code in ("code", arg) :: bnds in
       `Assoc bnds : error -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_response_error
and _ = yojson_of_error
[@@@end]


  type t =
    | Response of response
    | Response_error of response_error

  let make id result =
    Response {id; result; jsonrpc="2.0"}

  let make_error id code message =
    Response_error {id; error = {code; message;}; jsonrpc="2.0"}

  let to_yojson = function
    | Response v -> yojson_of_response v
    | Response_error v -> yojson_of_response_error v
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

  let yojson_of_params = function
    | PublishDiagnostics params -> PublishDiagnostics.yojson_of_params params

end

let send_notification rpc notif =
  let method_ = Server_notification.method_ notif in
  let params = Server_notification.yojson_of_params notif in
  let response = `Assoc [("jsonrpc", (`String "2.0")); ("method", (`String method_)); ("params", params)] in
  send rpc response

module Client_notification = struct
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.t
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
    | UnknownRequest : string * Yojson.Safe.t -> unit t

  let request_result_to_response (type a) id (req : a t) (result : a) =
    match req, result with
    | Shutdown, _resp -> None
    | TextDocumentHover _, result ->
      let json = Hover.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentDefinition _, result ->
      let json = Definition.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentTypeDefinition _, result ->
      let json = TypeDefinition.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentCompletion _, result ->
      let json = Completion.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentCodeLens _, result ->
      let json = CodeLens.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentRename _, result ->
      let json = Rename.yojson_of_result result in
      Some (Response.make id json)
    | DocumentSymbol _, result ->
      let json = TextDocumentDocumentSymbol.yojson_of_result result in
      Some (Response.make id json)
    | DebugEcho _, result ->
      let json = DebugEcho.yojson_of_result result in
      Some (Response.make id json)
    | DebugTextDocumentGet _, result ->
      let json = DebugTextDocumentGet.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentReferences _, result ->
      let json = References.yojson_of_result result in
      Some (Response.make id json)
    | TextDocumentHighlight _, result ->
      let json = TextDocumentHighlight.yojson_of_result result in
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
    let parse_yojson f v =
      match f v with
      | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure msg, _) ->
        Error msg
      | r -> Ok r
    in
    match packet.Packet.id with
    | Some id ->
      begin match packet.method_ with
      | "initialize" ->
        parse_yojson Protocol.Initialize.params_of_yojson packet.params
        >>| fun params -> Initialize (id, params)
      | "shutdown" ->
        Ok (Request (id, Shutdown))
      | "textDocument/completion" ->
        parse_yojson Completion.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentCompletion params)
      | "textDocument/documentSymbol" ->
        parse_yojson TextDocumentDocumentSymbol.params_of_yojson packet.params
        >>| fun params -> Request (id, DocumentSymbol params)
      | "textDocument/hover" ->
        parse_yojson Hover.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentHover params)
      | "textDocument/definition" ->
        parse_yojson Definition.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentDefinition params)
      | "textDocument/typeDefinition" ->
        parse_yojson TypeDefinition.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentTypeDefinition params)
      | "textDocument/references" ->
        parse_yojson References.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentReferences params)
      | "textDocument/codeLens" ->
        parse_yojson CodeLens.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentCodeLens params)
      | "textDocument/rename" ->
        parse_yojson Rename.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentRename params)
      | "textDocument/documentHighlight" ->
        parse_yojson TextDocumentHighlight.params_of_yojson packet.params
        >>| fun params -> Request (id, TextDocumentHighlight params)
      | "debug/echo" ->
        parse_yojson DebugEcho.params_of_yojson packet.params
        >>| fun params -> Request (id, DebugEcho params)
      | "debug/textDocument/get" ->
        parse_yojson DebugTextDocumentGet.params_of_yojson packet.params
        >>| fun params -> Request (id, DebugTextDocumentGet params)
      | name ->
        Ok (Request (id, UnknownRequest (name, packet.params)))
      end
    | None ->
      begin match packet.method_ with
      | "textDocument/didOpen" ->
        parse_yojson DidOpen.params_of_yojson packet.params
        >>| fun params -> Client_notification (TextDocumentDidOpen params)
      | "textDocument/didChange" ->
        parse_yojson DidChange.params_of_yojson packet.params
        >>| fun params -> Client_notification (TextDocumentDidChange params)
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
    read rpc >>= fun packet -> Message.parse packet
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
            let json = Protocol.Initialize.yojson_of_result result in
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
