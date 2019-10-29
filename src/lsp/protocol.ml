(**
 * This encodes LSP protocol specification as document at
 *
 *   https://microsoft.github.io/language-server-protocol/specification
 *
 * Most of this was borrowed from facebook/flow repository.
 *
 *)

let yojson_error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

type documentUri = Uri.t
[@@deriving_inline yojson]
let _ = fun (_ : documentUri) -> ()
let documentUri_of_yojson =
  (Uri.t_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> documentUri)
let _ = documentUri_of_yojson
let yojson_of_documentUri =
  (Uri.yojson_of_t : documentUri -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_documentUri
[@@@end]

type zero_based_int = int
[@@deriving_inline yojson]
let _ = fun (_ : zero_based_int) -> ()
let zero_based_int_of_yojson =
  (int_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> zero_based_int)
let _ = zero_based_int_of_yojson
let yojson_of_zero_based_int =
  (yojson_of_int : zero_based_int -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_zero_based_int
[@@@end]

type position = {
  line: zero_based_int;
  character: zero_based_int;
}
[@@deriving_inline yojson] [@@yojson.allow_extra_fields]
let _ = fun (_ : position) -> ()
let position_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.position" in
   function
   | `Assoc field_yojsons as yojson ->
       let line_field = ref None
       and character_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "line" ->
                   (match Ppx_yojson_conv_lib.(!) line_field with
                    | None ->
                        let fvalue = zero_based_int_of_yojson _field_yojson in
                        line_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "character" ->
                   (match Ppx_yojson_conv_lib.(!) character_field with
                    | None ->
                        let fvalue = zero_based_int_of_yojson _field_yojson in
                        character_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) line_field),
                           (Ppx_yojson_conv_lib.(!) character_field))
                   with
                   | (Some line_value, Some character_value) ->
                       { line = line_value; character = character_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) line_field) None),
                            "line");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) character_field) None),
                           "character")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> position)
let _ = position_of_yojson
let yojson_of_position =
  (function
   | { line = v_line; character = v_character } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_zero_based_int v_character in ("character", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_zero_based_int v_line in ("line", arg) :: bnds in
       `Assoc bnds : position -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_position
[@@@end]

type range = {
  start_: position [@key "start"];
  end_: position [@key "end"];
}
[@@deriving_inline yojson] [@@yojson.allow_extra_fields]
let _ = fun (_ : range) -> ()
let range_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.range" in
   function
   | `Assoc field_yojsons as yojson ->
       let start__field = ref None
       and end__field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "start" ->
                   (match Ppx_yojson_conv_lib.(!) start__field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        start__field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "end" ->
                   (match Ppx_yojson_conv_lib.(!) end__field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        end__field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) start__field),
                           (Ppx_yojson_conv_lib.(!) end__field))
                   with
                   | (Some start__value, Some end__value) ->
                       { start_ = start__value; end_ = end__value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) start__field) None),
                            "start_");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) end__field) None),
                           "end_")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> range)
let _ = range_of_yojson
let yojson_of_range =
  (function
   | { start_ = v_start_; end_ = v_end_ } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_position v_end_ in ("end", arg) :: bnds in
       let bnds =
         let arg = yojson_of_position v_start_ in ("start", arg) :: bnds in
       `Assoc bnds : range -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_range
[@@@end]

module Command = struct
  type t = {
    title : string;
    command : string;
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Command.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let title_field = ref None
       and command_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "title" ->
                   (match Ppx_yojson_conv_lib.(!) title_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        title_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "command" ->
                   (match Ppx_yojson_conv_lib.(!) command_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        command_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) title_field),
                           (Ppx_yojson_conv_lib.(!) command_field))
                   with
                   | (Some title_value, Some command_value) ->
                       { title = title_value; command = command_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) title_field) None),
                            "title");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) command_field) None),
                           "command")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { title = v_title; command = v_command } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_command in ("command", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_title in ("title", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

module MarkupKind = struct
  type t =
    | Plaintext
    | Markdown

  let yojson_of_t = function
    | Plaintext -> `String "plaintext"
    | Markdown -> `String "markdown"

  let t_of_yojson = function
    | `String "plaintext" -> Plaintext
    | `String "markdown" -> Markdown
    | `String _ -> Plaintext
    | node -> yojson_error "invalid contentFormat" node
end

module MarkupContent = struct
  type t = {
    value: string;
    kind: MarkupKind.t;
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.MarkupContent.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let value_field = ref None
       and kind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "value" ->
                   (match Ppx_yojson_conv_lib.(!) value_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        value_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "kind" ->
                   (match Ppx_yojson_conv_lib.(!) kind_field with
                    | None ->
                        let fvalue = MarkupKind.t_of_yojson _field_yojson in
                        kind_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) value_field),
                           (Ppx_yojson_conv_lib.(!) kind_field))
                   with
                   | (Some value_value, Some kind_value) ->
                       { value = value_value; kind = kind_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) value_field) None),
                            "value");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) kind_field) None),
                           "kind")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { value = v_value; kind = v_kind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = MarkupKind.yojson_of_t v_kind in ("kind", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_value in ("value", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

module Location = struct
  type t = {
    uri: Uri.t;
    range : range;
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Location.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = Uri.t_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) uri_field),
                           (Ppx_yojson_conv_lib.(!) range_field))
                   with
                   | (Some uri_value, Some range_value) ->
                       { uri = uri_value; range = range_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) range_field) None),
                           "range")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { uri = v_uri; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       let bnds = let arg = Uri.yojson_of_t v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end] 
end

module DefinitionLocation = struct
  type t = {
    uri: Uri.t;
    range : range;
    title: string option [@default None];
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DefinitionLocation.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and range_field = ref None
       and title_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = Uri.t_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "title" ->
                   (match Ppx_yojson_conv_lib.(!) title_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        title_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) uri_field),
                           (Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) title_field))
                   with
                   | (Some uri_value, Some range_value, title_value) ->
                       {
                         uri = uri_value;
                         range = range_value;
                         title =
                           ((match title_value with
                             | None -> None
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) range_field) None),
                           "range")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { uri = v_uri; range = v_range; title = v_title } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_title in
         ("title", arg) :: bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       let bnds = let arg = Uri.yojson_of_t v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

(* Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentIdentifier.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = documentUri_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) uri_field with
                   | Some uri_value -> { uri = uri_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { uri = v_uri } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_documentUri v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

(* An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    version: int;  (* the version number of this document *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.VersionedTextDocumentIdentifier.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and version_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = documentUri_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "version" ->
                   (match Ppx_yojson_conv_lib.(!) version_field with
                    | None ->
                        let fvalue = int_of_yojson _field_yojson in
                        version_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) uri_field),
                           (Ppx_yojson_conv_lib.(!) version_field))
                   with
                   | (Some uri_value, Some version_value) ->
                       { uri = uri_value; version = version_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) version_field) None),
                           "version")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { uri = v_uri; version = v_version } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_version in ("version", arg) :: bnds in
       let bnds =
         let arg = yojson_of_documentUri v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

(* An item to transfer a text document from the client to the server. The
   version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    languageId: string;  (* the text document's language identifier *)
    version: int;  (* the version of the document *)
    text: string;  (* the content of the opened text document *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentItem.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and languageId_field = ref None
       and version_field = ref None
       and text_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = documentUri_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "languageId" ->
                   (match Ppx_yojson_conv_lib.(!) languageId_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        languageId_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "version" ->
                   (match Ppx_yojson_conv_lib.(!) version_field with
                    | None ->
                        let fvalue = int_of_yojson _field_yojson in
                        version_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "text" ->
                   (match Ppx_yojson_conv_lib.(!) text_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        text_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) uri_field),
                           (Ppx_yojson_conv_lib.(!) languageId_field),
                           (Ppx_yojson_conv_lib.(!) version_field),
                           (Ppx_yojson_conv_lib.(!) text_field))
                   with
                   | (Some uri_value, Some languageId_value, Some
                      version_value, Some text_value) ->
                       {
                         uri = uri_value;
                         languageId = languageId_value;
                         version = version_value;
                         text = text_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) languageId_field) None),
                           "languageId");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) version_field) None),
                           "version");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) text_field) None),
                           "text")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { uri = v_uri; languageId = v_languageId; version = v_version;
       text = v_text } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_string v_text in ("text", arg) :: bnds in
       let bnds =
         let arg = yojson_of_int v_version in ("version", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_languageId in ("languageId", arg) ::
           bnds in
       let bnds =
         let arg = yojson_of_documentUri v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

(* DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = {
    textDocument: TextDocumentItem.t;  (* the document that was opened *)
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : didOpenTextDocumentParams) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DidOpen.params" in
   fun t -> didOpenTextDocumentParams_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                      -> params)
and didOpenTextDocumentParams_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DidOpen.didOpenTextDocumentParams" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentItem.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) textDocument_field with
                   | Some textDocument_value ->
                       { textDocument = textDocument_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    didOpenTextDocumentParams)
let _ = params_of_yojson
and _ = didOpenTextDocumentParams_of_yojson
let rec yojson_of_params =
  (fun v -> yojson_of_didOpenTextDocumentParams v : params ->
                                                      Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_didOpenTextDocumentParams =
  (function
   | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentItem.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : didOpenTextDocumentParams ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_didOpenTextDocumentParams
[@@@end]
end

(* DidChangeTextDocument notification, method="textDocument/didChange" *)
module DidChange = struct
  type params = didChangeTextDocumentParams

  and didChangeTextDocumentParams = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanges: textDocumentContentChangeEvent list;
  } [@@yojson.allow_extra_fields]

  and textDocumentContentChangeEvent = {
    range: range option [@default None]; (* the range of the document that changed *)
    rangeLength: int option [@default None]; (* the length that got replaced *)
    text: string; (* the new text of the range/document *)
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : didChangeTextDocumentParams) -> ()
let _ = fun (_ : textDocumentContentChangeEvent) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DidChange.params" in
   fun t -> didChangeTextDocumentParams_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                        -> params)
and didChangeTextDocumentParams_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DidChange.didChangeTextDocumentParams" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and contentChanges_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          VersionedTextDocumentIdentifier.t_of_yojson
                            _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "contentChanges" ->
                   (match Ppx_yojson_conv_lib.(!) contentChanges_field with
                    | None ->
                        let fvalue =
                          list_of_yojson
                            textDocumentContentChangeEvent_of_yojson
                            _field_yojson in
                        contentChanges_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) contentChanges_field))
                   with
                   | (Some textDocument_value, Some contentChanges_value) ->
                       {
                         textDocument = textDocument_value;
                         contentChanges = contentChanges_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) contentChanges_field)
                             None), "contentChanges")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    didChangeTextDocumentParams)
and textDocumentContentChangeEvent_of_yojson =
  (let _tp_loc =
     "src/lsp/protocol.ml.DidChange.textDocumentContentChangeEvent" in
   function
   | `Assoc field_yojsons as yojson ->
       let range_field = ref None
       and rangeLength_field = ref None
       and text_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue =
                          option_of_yojson range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "rangeLength" ->
                   (match Ppx_yojson_conv_lib.(!) rangeLength_field with
                    | None ->
                        let fvalue =
                          option_of_yojson int_of_yojson _field_yojson in
                        rangeLength_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "text" ->
                   (match Ppx_yojson_conv_lib.(!) text_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        text_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) rangeLength_field),
                           (Ppx_yojson_conv_lib.(!) text_field))
                   with
                   | (range_value, rangeLength_value, Some text_value) ->
                       {
                         range =
                           ((match range_value with
                             | None -> None
                             | Some v -> v));
                         rangeLength =
                           ((match rangeLength_value with
                             | None -> None
                             | Some v -> v));
                         text = text_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) text_field) None),
                            "text")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    textDocumentContentChangeEvent)
let _ = params_of_yojson
and _ = didChangeTextDocumentParams_of_yojson
and _ = textDocumentContentChangeEvent_of_yojson
let rec yojson_of_params =
  (fun v -> yojson_of_didChangeTextDocumentParams v : params ->
                                                        Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_didChangeTextDocumentParams =
  (function
   | { textDocument = v_textDocument; contentChanges = v_contentChanges } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           yojson_of_list yojson_of_textDocumentContentChangeEvent
             v_contentChanges in
         ("contentChanges", arg) :: bnds in
       let bnds =
         let arg = VersionedTextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : didChangeTextDocumentParams ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_textDocumentContentChangeEvent =
  (function
   | { range = v_range; rangeLength = v_rangeLength; text = v_text } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_string v_text in ("text", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_int v_rangeLength in
         ("rangeLength", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_range v_range in ("range", arg)
           :: bnds in
       `Assoc bnds : textDocumentContentChangeEvent ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_didChangeTextDocumentParams
and _ = yojson_of_textDocumentContentChangeEvent
[@@@end]
end

module TextDocumentPositionParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentPositionParams.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and position_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "position" ->
                   (match Ppx_yojson_conv_lib.(!) position_field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        position_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) position_field))
                   with
                   | (Some textDocument_value, Some position_value) ->
                       {
                         textDocument = textDocument_value;
                         position = position_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) position_field) None),
                           "position")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { textDocument = v_textDocument; position = v_position } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_position v_position in ("position", arg) :: bnds in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
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

  let yojson_of_kind = function
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let kind_of_yojson = function
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | node -> yojson_error "kind expected to be an int between 1 and 3" node

  type t = {
    range: range;
    kind: kind option;
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DocumentHighlight.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let range_field = ref None
       and kind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "kind" ->
                   (match Ppx_yojson_conv_lib.(!) kind_field with
                    | None ->
                        let fvalue =
                          option_of_yojson kind_of_yojson _field_yojson in
                        kind_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) kind_field))
                   with
                   | (Some range_value, Some kind_value) ->
                       { range = range_value; kind = kind_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) range_field) None),
                            "range");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) kind_field) None),
                           "kind")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { range = v_range; kind = v_kind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_kind v_kind in ("kind", arg) ::
           bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]

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
    range: range;
    (** The range of the text document to be manipulated. To insert text into
        a document create a range where start === end. *)
    newText: string;
    (** The string to be inserted. For delete operations use an empty string. *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextEdit.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let range_field = ref None
       and newText_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "newText" ->
                   (match Ppx_yojson_conv_lib.(!) newText_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        newText_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) newText_field))
                   with
                   | (Some range_value, Some newText_value) ->
                       { range = range_value; newText = newText_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) range_field) None),
                            "range");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) newText_field) None),
                           "newText")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { range = v_range; newText = v_newText } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_newText in ("newText", arg) :: bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
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
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentEdit.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and edits_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          VersionedTextDocumentIdentifier.t_of_yojson
                            _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "edits" ->
                   (match Ppx_yojson_conv_lib.(!) edits_field with
                    | None ->
                        let fvalue =
                          list_of_yojson TextEdit.t_of_yojson _field_yojson in
                        edits_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) edits_field))
                   with
                   | (Some textDocument_value, Some edits_value) ->
                       {
                         textDocument = textDocument_value;
                         edits = edits_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) edits_field) None),
                           "edits")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { textDocument = v_textDocument; edits = v_edits } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list TextEdit.yojson_of_t v_edits in
         ("edits", arg) :: bnds in
       let bnds =
         let arg = VersionedTextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
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

  let yojson_of_changes changes =
    let changes =
      List.map (fun (uri, edits) ->
        let uri = Uri.to_string uri in
        let edits = `List (List.map TextEdit.yojson_of_t edits) in
        uri, edits
      ) changes
    in
    `Assoc changes

  type documentChanges = TextDocumentEdit.t list
  [@@deriving_inline yojson_of]
  
let _ = fun (_ : documentChanges) -> ()
let yojson_of_documentChanges =
  (fun v -> yojson_of_list TextDocumentEdit.yojson_of_t v : documentChanges
                                                              ->
                                                              Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_documentChanges
[@@@end]

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
  }
  [@@deriving_inline yojson_of] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : t) -> ()
let yojson_of_t =
  (function
   | { changes = v_changes; documentChanges = v_documentChanges } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           yojson_of_option yojson_of_documentChanges v_documentChanges in
         ("documentChanges", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_changes v_changes in
         ("changes", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]

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

  let yojson_of_diagnosticCode = function
    | IntCode v -> `Int v
    | StringCode v -> `String v
    | NoCode -> `Null

  let diagnosticCode_of_yojson = function
    | `Int v -> (IntCode v)
    | `String v -> (StringCode v)
    | `Null -> NoCode
    | node -> yojson_error "invalid diagnostic.code" node

  type diagnosticSeverity =
    | Error (* 1 *)
    | Warning (* 2 *)
    | Information (* 3 *)
    | Hint (* 4 *)

  let yojson_of_diagnosticSeverity = function
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let diagnosticSeverity_of_yojson = function
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | node -> yojson_error "diagnostic.severity expected to be int" node

  type params = publishDiagnosticsParams

  and publishDiagnosticsParams = {
    uri: documentUri;
    diagnostics: diagnostic list;
  } [@@yojson.allow_extra_fields]

  and diagnostic = {
    range: range;  (* the range at which the message applies *)
    severity: diagnosticSeverity option [@default None];  (* if omitted, client decides *)
    code: diagnosticCode [@default NoCode];  (* the diagnostic's code. *)
    source: string option [@default None];  (* human-readable string, eg. typescript/lint *)
    message: string;  (* the diagnostic's message *)
    relatedInformation: diagnosticRelatedInformation list;
    relatedLocations: relatedLocation list; (* legacy FB extension *)
  } [@@yojson.allow_extra_fields]

  and diagnosticRelatedInformation = {
    relatedLocation: Location.t;  (* wire: just "location" *)
    relatedMessage: string;  (* wire: just "message" *)
  } [@@yojson.allow_extra_fields]

  (* legacy FB extension *)
  and relatedLocation = diagnosticRelatedInformation
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : publishDiagnosticsParams) -> ()
let _ = fun (_ : diagnostic) -> ()
let _ = fun (_ : diagnosticRelatedInformation) -> ()
let _ = fun (_ : relatedLocation) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.PublishDiagnostics.params" in
   fun t -> publishDiagnosticsParams_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                     -> params)
and publishDiagnosticsParams_of_yojson =
  (let _tp_loc =
     "src/lsp/protocol.ml.PublishDiagnostics.publishDiagnosticsParams" in
   function
   | `Assoc field_yojsons as yojson ->
       let uri_field = ref None
       and diagnostics_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "uri" ->
                   (match Ppx_yojson_conv_lib.(!) uri_field with
                    | None ->
                        let fvalue = documentUri_of_yojson _field_yojson in
                        uri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "diagnostics" ->
                   (match Ppx_yojson_conv_lib.(!) diagnostics_field with
                    | None ->
                        let fvalue =
                          list_of_yojson diagnostic_of_yojson _field_yojson in
                        diagnostics_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) uri_field),
                           (Ppx_yojson_conv_lib.(!) diagnostics_field))
                   with
                   | (Some uri_value, Some diagnostics_value) ->
                       { uri = uri_value; diagnostics = diagnostics_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) uri_field) None),
                            "uri");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) diagnostics_field) None),
                           "diagnostics")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    publishDiagnosticsParams)
and diagnostic_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.PublishDiagnostics.diagnostic" in
   function
   | `Assoc field_yojsons as yojson ->
       let range_field = ref None
       and severity_field = ref None
       and code_field = ref None
       and source_field = ref None
       and message_field = ref None
       and relatedInformation_field = ref None
       and relatedLocations_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "severity" ->
                   (match Ppx_yojson_conv_lib.(!) severity_field with
                    | None ->
                        let fvalue =
                          option_of_yojson diagnosticSeverity_of_yojson
                            _field_yojson in
                        severity_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "code" ->
                   (match Ppx_yojson_conv_lib.(!) code_field with
                    | None ->
                        let fvalue = diagnosticCode_of_yojson _field_yojson in
                        code_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "source" ->
                   (match Ppx_yojson_conv_lib.(!) source_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        source_field := (Some fvalue)
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
               | "relatedInformation" ->
                   (match Ppx_yojson_conv_lib.(!) relatedInformation_field
                    with
                    | None ->
                        let fvalue =
                          list_of_yojson
                            diagnosticRelatedInformation_of_yojson
                            _field_yojson in
                        relatedInformation_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "relatedLocations" ->
                   (match Ppx_yojson_conv_lib.(!) relatedLocations_field with
                    | None ->
                        let fvalue =
                          list_of_yojson relatedLocation_of_yojson
                            _field_yojson in
                        relatedLocations_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) severity_field),
                           (Ppx_yojson_conv_lib.(!) code_field),
                           (Ppx_yojson_conv_lib.(!) source_field),
                           (Ppx_yojson_conv_lib.(!) message_field),
                           (Ppx_yojson_conv_lib.(!) relatedInformation_field),
                           (Ppx_yojson_conv_lib.(!) relatedLocations_field))
                   with
                   | (Some range_value, severity_value, code_value,
                      source_value, Some message_value, Some
                      relatedInformation_value, Some relatedLocations_value)
                       ->
                       {
                         range = range_value;
                         severity =
                           ((match severity_value with
                             | None -> None
                             | Some v -> v));
                         code =
                           ((match code_value with
                             | None -> NoCode
                             | Some v -> v));
                         source =
                           ((match source_value with
                             | None -> None
                             | Some v -> v));
                         message = message_value;
                         relatedInformation = relatedInformation_value;
                         relatedLocations = relatedLocations_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) range_field) None),
                            "range");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) message_field) None),
                           "message");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                relatedInformation_field) None),
                           "relatedInformation");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) relatedLocations_field)
                             None), "relatedLocations")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> diagnostic)
and diagnosticRelatedInformation_of_yojson =
  (let _tp_loc =
     "src/lsp/protocol.ml.PublishDiagnostics.diagnosticRelatedInformation" in
   function
   | `Assoc field_yojsons as yojson ->
       let relatedLocation_field = ref None
       and relatedMessage_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "relatedLocation" ->
                   (match Ppx_yojson_conv_lib.(!) relatedLocation_field with
                    | None ->
                        let fvalue = Location.t_of_yojson _field_yojson in
                        relatedLocation_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "relatedMessage" ->
                   (match Ppx_yojson_conv_lib.(!) relatedMessage_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        relatedMessage_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) relatedLocation_field),
                           (Ppx_yojson_conv_lib.(!) relatedMessage_field))
                   with
                   | (Some relatedLocation_value, Some relatedMessage_value)
                       ->
                       {
                         relatedLocation = relatedLocation_value;
                         relatedMessage = relatedMessage_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) relatedLocation_field)
                              None), "relatedLocation");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) relatedMessage_field)
                             None), "relatedMessage")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    diagnosticRelatedInformation)
and relatedLocation_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.PublishDiagnostics.relatedLocation" in
   fun t -> diagnosticRelatedInformation_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> relatedLocation)
let _ = params_of_yojson
and _ = publishDiagnosticsParams_of_yojson
and _ = diagnostic_of_yojson
and _ = diagnosticRelatedInformation_of_yojson
and _ = relatedLocation_of_yojson
let rec yojson_of_params =
  (fun v -> yojson_of_publishDiagnosticsParams v : params ->
                                                     Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_publishDiagnosticsParams =
  (function
   | { uri = v_uri; diagnostics = v_diagnostics } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_diagnostic v_diagnostics in
         ("diagnostics", arg) :: bnds in
       let bnds =
         let arg = yojson_of_documentUri v_uri in ("uri", arg) :: bnds in
       `Assoc bnds : publishDiagnosticsParams ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_diagnostic =
  (function
   | { range = v_range; severity = v_severity; code = v_code;
       source = v_source; message = v_message;
       relatedInformation = v_relatedInformation;
       relatedLocations = v_relatedLocations } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg =
           yojson_of_list yojson_of_relatedLocation v_relatedLocations in
         ("relatedLocations", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_list yojson_of_diagnosticRelatedInformation
             v_relatedInformation in
         ("relatedInformation", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_message in ("message", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_source in
         ("source", arg) :: bnds in
       let bnds =
         let arg = yojson_of_diagnosticCode v_code in ("code", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_diagnosticSeverity v_severity in
         ("severity", arg) :: bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       `Assoc bnds : diagnostic -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_diagnosticRelatedInformation =
  (function
   | { relatedLocation = v_relatedLocation; relatedMessage = v_relatedMessage
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_relatedMessage in
         ("relatedMessage", arg) :: bnds in
       let bnds =
         let arg = Location.yojson_of_t v_relatedLocation in
         ("relatedLocation", arg) :: bnds in
       `Assoc bnds : diagnosticRelatedInformation ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_relatedLocation =
  (fun v -> yojson_of_diagnosticRelatedInformation v : relatedLocation ->
                                                         Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_publishDiagnosticsParams
and _ = yojson_of_diagnostic
and _ = yojson_of_diagnosticRelatedInformation
and _ = yojson_of_relatedLocation
[@@@end]
end

(* Completion request, method="textDocument/completion" *)
module Completion = struct

  type completionTriggerKind =
    | Invoked (* 1 *)
    | TriggerCharacter (* 2 *)
    | TriggerForIncompleteCompletions (* 3 *)

  let yojson_of_completionTriggerKind = function
    | Invoked -> `Int 1
    | TriggerCharacter -> `Int 2
    | TriggerForIncompleteCompletions -> `Int 3

  let completionTriggerKind_of_yojson = function
    | `Int 1 -> Invoked
    | `Int 2 -> TriggerCharacter
    | `Int 3 -> TriggerForIncompleteCompletions
    | v -> yojson_error "invalid completion.triggerKind, should be equal to 1, 2 or 3" v

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

  let yojson_of_completionItemKind v =
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
      | Some v -> v
      | None -> yojson_error "completion.kind expected to be between 1 and 25"
                  (`Int v)
      end
    | node -> yojson_error "completion.kind expected to be between 1 and 25"
                node

    (** Keep this in sync with `int_of_completionItemKind`. *)
  type insertTextFormat =
    | PlainText (* 1 *)  (* the insertText/textEdits are just plain strings *)
    | SnippetFormat (* 2 *)  (* wire: just "Snippet" *)

  (** Once we get better PPX support we can use [@@deriving enum].
    Keep in sync with insertFormat_of_int_opt. *)
  let int_of_insertFormat = function
    | PlainText -> 1
    | SnippetFormat -> 2

  let yojson_of_insertTextFormat v =
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
      | Some v -> v
      | None -> yojson_error "insertTextFormat expected to be 1 or 2" (`Int v)
      end
    | node -> yojson_error "insertTextFormat expected to be 1 or 2" node

  type params = completionParams

  and completionParams = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: completionContext option [@default None];
  } [@@yojson.allow_extra_fields]

  and completionContext = {
    triggerKind: completionTriggerKind;
  } [@@yojson.allow_extra_fields]

  and result = completionList  (* wire: can also be 'completionItem list' *)

  and completionList = {
    isIncomplete: bool; (* further typing should result in recomputing *)
    items: completionItem list;
  } [@@yojson.allow_extra_fields]

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
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : completionParams) -> ()
let _ = fun (_ : completionContext) -> ()
let _ = fun (_ : result) -> ()
let _ = fun (_ : completionList) -> ()
let _ = fun (_ : completionItem) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.params" in
   fun t -> completionParams_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                             -> params)
and completionParams_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.completionParams" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and position_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "position" ->
                   (match Ppx_yojson_conv_lib.(!) position_field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        position_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "context" ->
                   (match Ppx_yojson_conv_lib.(!) context_field with
                    | None ->
                        let fvalue =
                          option_of_yojson completionContext_of_yojson
                            _field_yojson in
                        context_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) position_field),
                           (Ppx_yojson_conv_lib.(!) context_field))
                   with
                   | (Some textDocument_value, Some position_value,
                      context_value) ->
                       {
                         textDocument = textDocument_value;
                         position = position_value;
                         context =
                           ((match context_value with
                             | None -> None
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) position_field) None),
                           "position")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionParams)
and completionContext_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.completionContext" in
   function
   | `Assoc field_yojsons as yojson ->
       let triggerKind_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "triggerKind" ->
                   (match Ppx_yojson_conv_lib.(!) triggerKind_field with
                    | None ->
                        let fvalue =
                          completionTriggerKind_of_yojson _field_yojson in
                        triggerKind_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) triggerKind_field with
                   | Some triggerKind_value ->
                       { triggerKind = triggerKind_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) triggerKind_field)
                              None), "triggerKind")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionContext)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.result" in
   fun t -> completionList_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                                           result)
and completionList_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.completionList" in
   function
   | `Assoc field_yojsons as yojson ->
       let isIncomplete_field = ref None
       and items_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "isIncomplete" ->
                   (match Ppx_yojson_conv_lib.(!) isIncomplete_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        isIncomplete_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "items" ->
                   (match Ppx_yojson_conv_lib.(!) items_field with
                    | None ->
                        let fvalue =
                          list_of_yojson completionItem_of_yojson
                            _field_yojson in
                        items_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) isIncomplete_field),
                           (Ppx_yojson_conv_lib.(!) items_field))
                   with
                   | (Some isIncomplete_value, Some items_value) ->
                       {
                         isIncomplete = isIncomplete_value;
                         items = items_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) isIncomplete_field)
                              None), "isIncomplete");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) items_field) None),
                           "items")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionList)
and completionItem_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Completion.completionItem" in
   function
   | `Assoc field_yojsons as yojson ->
       let label_field = ref None
       and kind_field = ref None
       and detail_field = ref None
       and inlineDetail_field = ref None
       and itemType_field = ref None
       and documentation_field = ref None
       and sortText_field = ref None
       and filterText_field = ref None
       and insertText_field = ref None
       and insertTextFormat_field = ref None
       and textEdit_field = ref None
       and additionalTextEdits_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "label" ->
                   (match Ppx_yojson_conv_lib.(!) label_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        label_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "kind" ->
                   (match Ppx_yojson_conv_lib.(!) kind_field with
                    | None ->
                        let fvalue =
                          option_of_yojson completionItemKind_of_yojson
                            _field_yojson in
                        kind_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "detail" ->
                   (match Ppx_yojson_conv_lib.(!) detail_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        detail_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "inlineDetail" ->
                   (match Ppx_yojson_conv_lib.(!) inlineDetail_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        inlineDetail_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "itemType" ->
                   (match Ppx_yojson_conv_lib.(!) itemType_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        itemType_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentation" ->
                   (match Ppx_yojson_conv_lib.(!) documentation_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        documentation_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "sortText" ->
                   (match Ppx_yojson_conv_lib.(!) sortText_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        sortText_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "filterText" ->
                   (match Ppx_yojson_conv_lib.(!) filterText_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        filterText_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "insertText" ->
                   (match Ppx_yojson_conv_lib.(!) insertText_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        insertText_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "insertTextFormat" ->
                   (match Ppx_yojson_conv_lib.(!) insertTextFormat_field with
                    | None ->
                        let fvalue =
                          option_of_yojson insertTextFormat_of_yojson
                            _field_yojson in
                        insertTextFormat_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "textEdit" ->
                   (match Ppx_yojson_conv_lib.(!) textEdit_field with
                    | None ->
                        let fvalue =
                          option_of_yojson TextEdit.t_of_yojson _field_yojson in
                        textEdit_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "additionalTextEdits" ->
                   (match Ppx_yojson_conv_lib.(!) additionalTextEdits_field
                    with
                    | None ->
                        let fvalue =
                          list_of_yojson TextEdit.t_of_yojson _field_yojson in
                        additionalTextEdits_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) label_field),
                           (Ppx_yojson_conv_lib.(!) kind_field),
                           (Ppx_yojson_conv_lib.(!) detail_field),
                           (Ppx_yojson_conv_lib.(!) inlineDetail_field),
                           (Ppx_yojson_conv_lib.(!) itemType_field),
                           (Ppx_yojson_conv_lib.(!) documentation_field),
                           (Ppx_yojson_conv_lib.(!) sortText_field),
                           (Ppx_yojson_conv_lib.(!) filterText_field),
                           (Ppx_yojson_conv_lib.(!) insertText_field),
                           (Ppx_yojson_conv_lib.(!) insertTextFormat_field),
                           (Ppx_yojson_conv_lib.(!) textEdit_field),
                           (Ppx_yojson_conv_lib.(!) additionalTextEdits_field))
                   with
                   | (Some label_value, kind_value, detail_value,
                      inlineDetail_value, itemType_value,
                      documentation_value, sortText_value, filterText_value,
                      insertText_value, insertTextFormat_value,
                      textEdit_value, additionalTextEdits_value) ->
                       {
                         label = label_value;
                         kind =
                           ((match kind_value with
                             | None -> None
                             | Some v -> v));
                         detail =
                           ((match detail_value with
                             | None -> None
                             | Some v -> v));
                         inlineDetail =
                           ((match inlineDetail_value with
                             | None -> None
                             | Some v -> v));
                         itemType =
                           ((match itemType_value with
                             | None -> None
                             | Some v -> v));
                         documentation =
                           ((match documentation_value with
                             | None -> None
                             | Some v -> v));
                         sortText =
                           ((match sortText_value with
                             | None -> None
                             | Some v -> v));
                         filterText =
                           ((match filterText_value with
                             | None -> None
                             | Some v -> v));
                         insertText =
                           ((match insertText_value with
                             | None -> None
                             | Some v -> v));
                         insertTextFormat =
                           ((match insertTextFormat_value with
                             | None -> None
                             | Some v -> v));
                         textEdit =
                           ((match textEdit_value with
                             | None -> None
                             | Some v -> v));
                         additionalTextEdits =
                           ((match additionalTextEdits_value with
                             | None -> []
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) label_field) None),
                            "label")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionItem)
let _ = params_of_yojson
and _ = completionParams_of_yojson
and _ = completionContext_of_yojson
and _ = result_of_yojson
and _ = completionList_of_yojson
and _ = completionItem_of_yojson
let rec yojson_of_params =
  (fun v -> yojson_of_completionParams v : params ->
                                             Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_completionParams =
  (function
   | { textDocument = v_textDocument; position = v_position;
       context = v_context } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_completionContext v_context in
         ("context", arg) :: bnds in
       let bnds =
         let arg = yojson_of_position v_position in ("position", arg) :: bnds in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : completionParams -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_completionContext =
  (function
   | { triggerKind = v_triggerKind } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_completionTriggerKind v_triggerKind in
         ("triggerKind", arg) :: bnds in
       `Assoc bnds : completionContext -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_completionList v : result ->
                                           Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_completionList =
  (function
   | { isIncomplete = v_isIncomplete; items = v_items } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_completionItem v_items in
         ("items", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_isIncomplete in ("isIncomplete", arg) ::
           bnds in
       `Assoc bnds : completionList -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_completionItem =
  (function
   | { label = v_label; kind = v_kind; detail = v_detail;
       inlineDetail = v_inlineDetail; itemType = v_itemType;
       documentation = v_documentation; sortText = v_sortText;
       filterText = v_filterText; insertText = v_insertText;
       insertTextFormat = v_insertTextFormat; textEdit = v_textEdit;
       additionalTextEdits = v_additionalTextEdits } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list TextEdit.yojson_of_t v_additionalTextEdits in
         ("additionalTextEdits", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option TextEdit.yojson_of_t v_textEdit in
         ("textEdit", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_insertTextFormat v_insertTextFormat in
         ("insertTextFormat", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_insertText in
         ("insertText", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_filterText in
         ("filterText", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_sortText in
         ("sortText", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_documentation in
         ("documentation", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_itemType in
         ("itemType", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_inlineDetail in
         ("inlineDetail", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_detail in
         ("detail", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_completionItemKind v_kind in
         ("kind", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_label in ("label", arg) :: bnds in
       `Assoc bnds : completionItem -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_completionParams
and _ = yojson_of_completionContext
and _ = yojson_of_result
and _ = yojson_of_completionList
and _ = yojson_of_completionItem
[@@@end]
end

(* Hover request, method="textDocument/hover" *)
module Hover = struct
  type params =
    TextDocumentPositionParams.t

  and result = hoverResult option [@default None]

  and hoverResult = {
    contents: MarkupContent.t;
    range: range option [@default None];
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let _ = fun (_ : hoverResult) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Hover.params" in
   fun t -> TextDocumentPositionParams.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Hover.result" in
   fun t -> option_of_yojson hoverResult_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> result)
and hoverResult_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Hover.hoverResult" in
   function
   | `Assoc field_yojsons as yojson ->
       let contents_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "contents" ->
                   (match Ppx_yojson_conv_lib.(!) contents_field with
                    | None ->
                        let fvalue = MarkupContent.t_of_yojson _field_yojson in
                        contents_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue =
                          option_of_yojson range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) contents_field),
                           (Ppx_yojson_conv_lib.(!) range_field))
                   with
                   | (Some contents_value, range_value) ->
                       {
                         contents = contents_value;
                         range =
                           ((match range_value with
                             | None -> None
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) contents_field) None),
                            "contents")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> hoverResult)
let _ = params_of_yojson
and _ = result_of_yojson
and _ = hoverResult_of_yojson
let rec yojson_of_params =
  (fun v -> TextDocumentPositionParams.yojson_of_t v : params ->
                                                         Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_option yojson_of_hoverResult v : result ->
                                                         Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_hoverResult =
  (function
   | { contents = v_contents; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_range v_range in ("range", arg)
           :: bnds in
       let bnds =
         let arg = MarkupContent.yojson_of_t v_contents in ("contents", arg)
           :: bnds in
       `Assoc bnds : hoverResult -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
and _ = yojson_of_hoverResult
[@@@end]
end

(* Initialize request, method="initialize" *)
module Initialize = struct

  type trace =
    | Off
    | Messages
    | Verbose

  let yojson_of_trace = function
    | Off -> `String "off"
    | Messages -> `String "messages"
    | Verbose -> `String "verbose"

  let trace_of_yojson = function
    | `String "off" -> Off
    | `String "messages" -> Messages
    | `String "verbose" -> Verbose
    | node -> yojson_error "invalid trace" node

  type textDocumentSyncKind =
    | NoSync (* 0 *)  (* docs should not be synced at all. Wire "None" *)
    | FullSync (* 1 *)  (* synced by always sending full content. Wire "Full" *)
    | IncrementalSync (* 2 *)  (* full only on open. Wire "Incremental" *)

  let yojson_of_textDocumentSyncKind = function
    | NoSync -> `Int 0
    | FullSync -> `Int 1
    | IncrementalSync -> `Int 2

  let textDocumentSyncKind_of_yojson = function
    | `Int 0 -> NoSync
    | `Int 1 -> FullSync
    | `Int 2 -> IncrementalSync
    | node -> yojson_error "invalid textDocumentSyncKind" node

  (* synchronization capabilities say what messages the client is capable
   * of sending, should be be so asked by the server.
   * We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)

  type synchronization = {
    willSave: bool [@default false];  (* client can send textDocument/willSave *)
    willSaveWaitUntil: bool [@default false];  (* textDoc.../willSaveWaitUntil *)
    didSave: bool [@default false];  (* textDocument/didSave *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : synchronization) -> ()
let synchronization_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.synchronization" in
   function
   | `Assoc field_yojsons as yojson ->
       let willSave_field = ref None
       and willSaveWaitUntil_field = ref None
       and didSave_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "willSave" ->
                   (match Ppx_yojson_conv_lib.(!) willSave_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        willSave_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "willSaveWaitUntil" ->
                   (match Ppx_yojson_conv_lib.(!) willSaveWaitUntil_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        willSaveWaitUntil_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "didSave" ->
                   (match Ppx_yojson_conv_lib.(!) didSave_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        didSave_field := (Some fvalue)
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
                  let (willSave_value, willSaveWaitUntil_value,
                       didSave_value)
                    =
                    ((Ppx_yojson_conv_lib.(!) willSave_field),
                      (Ppx_yojson_conv_lib.(!) willSaveWaitUntil_field),
                      (Ppx_yojson_conv_lib.(!) didSave_field)) in
                  {
                    willSave =
                      ((match willSave_value with
                        | None -> false
                        | Some v -> v));
                    willSaveWaitUntil =
                      ((match willSaveWaitUntil_value with
                        | None -> false
                        | Some v -> v));
                    didSave =
                      ((match didSave_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> synchronization)
let _ = synchronization_of_yojson
let yojson_of_synchronization =
  (function
   | { willSave = v_willSave; willSaveWaitUntil = v_willSaveWaitUntil;
       didSave = v_didSave } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_didSave in ("didSave", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_willSaveWaitUntil in
         ("willSaveWaitUntil", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_willSave in ("willSave", arg) :: bnds in
       `Assoc bnds : synchronization -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_synchronization
[@@@end]

  let synchronization_empty = {
    willSave = true;
    willSaveWaitUntil = true;
    didSave = true;
  }

  type completionItem = {
    snippetSupport: bool [@default false];  (* client can do snippets as insert text *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : completionItem) -> ()
let completionItem_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.completionItem" in
   function
   | `Assoc field_yojsons as yojson ->
       let snippetSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "snippetSupport" ->
                   (match Ppx_yojson_conv_lib.(!) snippetSupport_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        snippetSupport_field := (Some fvalue)
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
                  let snippetSupport_value =
                    Ppx_yojson_conv_lib.(!) snippetSupport_field in
                  {
                    snippetSupport =
                      ((match snippetSupport_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionItem)
let _ = completionItem_of_yojson
let yojson_of_completionItem =
  (function
   | { snippetSupport = v_snippetSupport } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_snippetSupport in ("snippetSupport", arg)
           :: bnds in
       `Assoc bnds : completionItem -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_completionItem
[@@@end]

  let completionItem_empty = {
    snippetSupport = false;
  }

  type completion = {
    completionItem: completionItem [@default completionItem_empty];
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : completion) -> ()
let completion_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.completion" in
   function
   | `Assoc field_yojsons as yojson ->
       let completionItem_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "completionItem" ->
                   (match Ppx_yojson_conv_lib.(!) completionItem_field with
                    | None ->
                        let fvalue = completionItem_of_yojson _field_yojson in
                        completionItem_field := (Some fvalue)
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
                  let completionItem_value =
                    Ppx_yojson_conv_lib.(!) completionItem_field in
                  {
                    completionItem =
                      ((match completionItem_value with
                        | None -> completionItem_empty
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completion)
let _ = completion_of_yojson
let yojson_of_completion =
  (function
   | { completionItem = v_completionItem } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_completionItem v_completionItem in
         ("completionItem", arg) :: bnds in
       `Assoc bnds : completion -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_completion
[@@@end]

  let completion_empty = {
    completionItem = completionItem_empty;
  }

  type hover = {
    contentFormat: MarkupKind.t list [@default [Plaintext]];
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : hover) -> ()
let hover_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.hover" in
   function
   | `Assoc field_yojsons as yojson ->
       let contentFormat_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "contentFormat" ->
                   (match Ppx_yojson_conv_lib.(!) contentFormat_field with
                    | None ->
                        let fvalue =
                          list_of_yojson MarkupKind.t_of_yojson _field_yojson in
                        contentFormat_field := (Some fvalue)
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
                  let contentFormat_value =
                    Ppx_yojson_conv_lib.(!) contentFormat_field in
                  {
                    contentFormat =
                      ((match contentFormat_value with
                        | None -> [Plaintext]
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> hover)
let _ = hover_of_yojson
let yojson_of_hover =
  (function
   | { contentFormat = v_contentFormat } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list MarkupKind.yojson_of_t v_contentFormat in
         ("contentFormat", arg) :: bnds in
       `Assoc bnds : hover -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_hover
[@@@end]

  let hover_empty = {
    contentFormat = [Plaintext];
  }

  type documentSymbol = {
    hierarchicalDocumentSymbolSupport : bool [@default false];
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : documentSymbol) -> ()
let documentSymbol_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.documentSymbol" in
   function
   | `Assoc field_yojsons as yojson ->
       let hierarchicalDocumentSymbolSupport_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "hierarchicalDocumentSymbolSupport" ->
                   (match Ppx_yojson_conv_lib.(!)
                            hierarchicalDocumentSymbolSupport_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        hierarchicalDocumentSymbolSupport_field :=
                          (Some fvalue)
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
                  let hierarchicalDocumentSymbolSupport_value =
                    Ppx_yojson_conv_lib.(!)
                      hierarchicalDocumentSymbolSupport_field in
                  {
                    hierarchicalDocumentSymbolSupport =
                      ((match hierarchicalDocumentSymbolSupport_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> documentSymbol)
let _ = documentSymbol_of_yojson
let yojson_of_documentSymbol =
  (function
   | {
       hierarchicalDocumentSymbolSupport =
         v_hierarchicalDocumentSymbolSupport
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_hierarchicalDocumentSymbolSupport in
         ("hierarchicalDocumentSymbolSupport", arg) :: bnds in
       `Assoc bnds : documentSymbol -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_documentSymbol
[@@@end]

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
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : textDocumentClientCapabilities) -> ()
let textDocumentClientCapabilities_of_yojson =
  (let _tp_loc =
     "src/lsp/protocol.ml.Initialize.textDocumentClientCapabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let synchronization_field = ref None
       and completion_field = ref None
       and documentSymbol_field = ref None
       and hover_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "synchronization" ->
                   (match Ppx_yojson_conv_lib.(!) synchronization_field with
                    | None ->
                        let fvalue = synchronization_of_yojson _field_yojson in
                        synchronization_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "completion" ->
                   (match Ppx_yojson_conv_lib.(!) completion_field with
                    | None ->
                        let fvalue = completion_of_yojson _field_yojson in
                        completion_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentSymbol" ->
                   (match Ppx_yojson_conv_lib.(!) documentSymbol_field with
                    | None ->
                        let fvalue = documentSymbol_of_yojson _field_yojson in
                        documentSymbol_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "hover" ->
                   (match Ppx_yojson_conv_lib.(!) hover_field with
                    | None ->
                        let fvalue = hover_of_yojson _field_yojson in
                        hover_field := (Some fvalue)
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
                  let (synchronization_value, completion_value,
                       documentSymbol_value, hover_value)
                    =
                    ((Ppx_yojson_conv_lib.(!) synchronization_field),
                      (Ppx_yojson_conv_lib.(!) completion_field),
                      (Ppx_yojson_conv_lib.(!) documentSymbol_field),
                      (Ppx_yojson_conv_lib.(!) hover_field)) in
                  {
                    synchronization =
                      ((match synchronization_value with
                        | None -> synchronization_empty
                        | Some v -> v));
                    completion =
                      ((match completion_value with
                        | None -> completion_empty
                        | Some v -> v));
                    documentSymbol =
                      ((match documentSymbol_value with
                        | None -> documentSymbol_empty
                        | Some v -> v));
                    hover =
                      ((match hover_value with
                        | None -> hover_empty
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    textDocumentClientCapabilities)
let _ = textDocumentClientCapabilities_of_yojson
let yojson_of_textDocumentClientCapabilities =
  (function
   | { synchronization = v_synchronization; completion = v_completion;
       documentSymbol = v_documentSymbol; hover = v_hover } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_hover v_hover in ("hover", arg) :: bnds in
       let bnds =
         let arg = yojson_of_documentSymbol v_documentSymbol in
         ("documentSymbol", arg) :: bnds in
       let bnds =
         let arg = yojson_of_completion v_completion in ("completion", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_synchronization v_synchronization in
         ("synchronization", arg) :: bnds in
       `Assoc bnds : textDocumentClientCapabilities ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_textDocumentClientCapabilities
[@@@end]

  let textDocumentClientCapabilities_empty = {
    completion = completion_empty;
    synchronization = synchronization_empty;
    hover = hover_empty;
    documentSymbol = documentSymbol_empty;
  }

  type workspaceEdit = {
    documentChanges: bool [@default false];
    (** client supports versioned doc changes *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : workspaceEdit) -> ()
let workspaceEdit_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.workspaceEdit" in
   function
   | `Assoc field_yojsons as yojson ->
       let documentChanges_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "documentChanges" ->
                   (match Ppx_yojson_conv_lib.(!) documentChanges_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        documentChanges_field := (Some fvalue)
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
                  let documentChanges_value =
                    Ppx_yojson_conv_lib.(!) documentChanges_field in
                  {
                    documentChanges =
                      ((match documentChanges_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> workspaceEdit)
let _ = workspaceEdit_of_yojson
let yojson_of_workspaceEdit =
  (function
   | { documentChanges = v_documentChanges } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_documentChanges in
         ("documentChanges", arg) :: bnds in
       `Assoc bnds : workspaceEdit -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_workspaceEdit
[@@@end]

  let workspaceEdit_empty = {
    documentChanges = false;
  }

  type workspaceClientCapabilities = {
    applyEdit: bool [@default false];
    (** client supports applying batch edits *)
    workspaceEdit: workspaceEdit [@default workspaceEdit_empty];
    (** omitted: dynamic-registration fields *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : workspaceClientCapabilities) -> ()
let workspaceClientCapabilities_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.workspaceClientCapabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let applyEdit_field = ref None
       and workspaceEdit_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "applyEdit" ->
                   (match Ppx_yojson_conv_lib.(!) applyEdit_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        applyEdit_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "workspaceEdit" ->
                   (match Ppx_yojson_conv_lib.(!) workspaceEdit_field with
                    | None ->
                        let fvalue = workspaceEdit_of_yojson _field_yojson in
                        workspaceEdit_field := (Some fvalue)
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
                  let (applyEdit_value, workspaceEdit_value) =
                    ((Ppx_yojson_conv_lib.(!) applyEdit_field),
                      (Ppx_yojson_conv_lib.(!) workspaceEdit_field)) in
                  {
                    applyEdit =
                      ((match applyEdit_value with
                        | None -> false
                        | Some v -> v));
                    workspaceEdit =
                      ((match workspaceEdit_value with
                        | None -> workspaceEdit_empty
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    workspaceClientCapabilities)
let _ = workspaceClientCapabilities_of_yojson
let yojson_of_workspaceClientCapabilities =
  (function
   | { applyEdit = v_applyEdit; workspaceEdit = v_workspaceEdit } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_workspaceEdit v_workspaceEdit in
         ("workspaceEdit", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_applyEdit in ("applyEdit", arg) :: bnds in
       `Assoc bnds : workspaceClientCapabilities ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_workspaceClientCapabilities
[@@@end]

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
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : windowClientCapabilities) -> ()
let windowClientCapabilities_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.windowClientCapabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let status_field = ref None
       and progress_field = ref None
       and actionRequired_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "status" ->
                   (match Ppx_yojson_conv_lib.(!) status_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        status_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "progress" ->
                   (match Ppx_yojson_conv_lib.(!) progress_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        progress_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "actionRequired" ->
                   (match Ppx_yojson_conv_lib.(!) actionRequired_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        actionRequired_field := (Some fvalue)
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
                  let (status_value, progress_value, actionRequired_value) =
                    ((Ppx_yojson_conv_lib.(!) status_field),
                      (Ppx_yojson_conv_lib.(!) progress_field),
                      (Ppx_yojson_conv_lib.(!) actionRequired_field)) in
                  {
                    status =
                      ((match status_value with | None -> false | Some v -> v));
                    progress =
                      ((match progress_value with
                        | None -> false
                        | Some v -> v));
                    actionRequired =
                      ((match actionRequired_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    windowClientCapabilities)
let _ = windowClientCapabilities_of_yojson
let yojson_of_windowClientCapabilities =
  (function
   | { status = v_status; progress = v_progress;
       actionRequired = v_actionRequired } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_actionRequired in ("actionRequired", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_bool v_progress in ("progress", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_status in ("status", arg) :: bnds in
       `Assoc bnds : windowClientCapabilities ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_windowClientCapabilities
[@@@end]

  let windowClientCapabilities_empty = {
    status = true;
    progress = true;
    actionRequired = true;
  }

  type telemetryClientCapabilities = {
    (* Nuclide-specific: client supports telemetry/connectionStatus *)
    connectionStatus: bool [@default false];
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : telemetryClientCapabilities) -> ()
let telemetryClientCapabilities_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.telemetryClientCapabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let connectionStatus_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "connectionStatus" ->
                   (match Ppx_yojson_conv_lib.(!) connectionStatus_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        connectionStatus_field := (Some fvalue)
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
                  let connectionStatus_value =
                    Ppx_yojson_conv_lib.(!) connectionStatus_field in
                  {
                    connectionStatus =
                      ((match connectionStatus_value with
                        | None -> false
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    telemetryClientCapabilities)
let _ = telemetryClientCapabilities_of_yojson
let yojson_of_telemetryClientCapabilities =
  (function
   | { connectionStatus = v_connectionStatus } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_connectionStatus in
         ("connectionStatus", arg) :: bnds in
       `Assoc bnds : telemetryClientCapabilities ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_telemetryClientCapabilities
[@@@end]

  let telemetryClientCapabilities_empty = {
    connectionStatus = true;
  }

  type client_capabilities = {
    workspace: workspaceClientCapabilities [@default workspaceClientCapabilities_empty];
    textDocument: textDocumentClientCapabilities [@default textDocumentClientCapabilities_empty];
    window: windowClientCapabilities [@default windowClientCapabilities_empty];
    telemetry: telemetryClientCapabilities [@default telemetryClientCapabilities_empty];
    (* omitted: experimental *)
  }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]
  
let _ = fun (_ : client_capabilities) -> ()
let client_capabilities_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.client_capabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let workspace_field = ref None
       and textDocument_field = ref None
       and window_field = ref None
       and telemetry_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "workspace" ->
                   (match Ppx_yojson_conv_lib.(!) workspace_field with
                    | None ->
                        let fvalue =
                          workspaceClientCapabilities_of_yojson _field_yojson in
                        workspace_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          textDocumentClientCapabilities_of_yojson
                            _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "window" ->
                   (match Ppx_yojson_conv_lib.(!) window_field with
                    | None ->
                        let fvalue =
                          windowClientCapabilities_of_yojson _field_yojson in
                        window_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "telemetry" ->
                   (match Ppx_yojson_conv_lib.(!) telemetry_field with
                    | None ->
                        let fvalue =
                          telemetryClientCapabilities_of_yojson _field_yojson in
                        telemetry_field := (Some fvalue)
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
                  let (workspace_value, textDocument_value, window_value,
                       telemetry_value)
                    =
                    ((Ppx_yojson_conv_lib.(!) workspace_field),
                      (Ppx_yojson_conv_lib.(!) textDocument_field),
                      (Ppx_yojson_conv_lib.(!) window_field),
                      (Ppx_yojson_conv_lib.(!) telemetry_field)) in
                  {
                    workspace =
                      ((match workspace_value with
                        | None -> workspaceClientCapabilities_empty
                        | Some v -> v));
                    textDocument =
                      ((match textDocument_value with
                        | None -> textDocumentClientCapabilities_empty
                        | Some v -> v));
                    window =
                      ((match window_value with
                        | None -> windowClientCapabilities_empty
                        | Some v -> v));
                    telemetry =
                      ((match telemetry_value with
                        | None -> telemetryClientCapabilities_empty
                        | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> client_capabilities)
let _ = client_capabilities_of_yojson
let yojson_of_client_capabilities =
  (function
   | { workspace = v_workspace; textDocument = v_textDocument;
       window = v_window; telemetry = v_telemetry } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_telemetryClientCapabilities v_telemetry in
         ("telemetry", arg) :: bnds in
       let bnds =
         let arg = yojson_of_windowClientCapabilities v_window in
         ("window", arg) :: bnds in
       let bnds =
         let arg = yojson_of_textDocumentClientCapabilities v_textDocument in
         ("textDocument", arg) :: bnds in
       let bnds =
         let arg = yojson_of_workspaceClientCapabilities v_workspace in
         ("workspace", arg) :: bnds in
       `Assoc bnds : client_capabilities -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_client_capabilities
[@@@end]

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
  } [@@yojson.allow_extra_fields]

  and result = {
    server_capabilities: server_capabilities [@key "capabilities"];
  } [@@yojson.allow_extra_fields]

  and errorData = {
    retry: bool;  (* should client retry the initialize request *)
  } [@@yojson.allow_extra_fields]

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
  } [@@yojson.allow_extra_fields]

  and completionOptions = {
    resolveProvider: bool;  (* server resolves extra info on demand *)
    triggerCharacters: string list; (* wire "triggerCharacters" *)
  } [@@yojson.allow_extra_fields]

  (* and signatureHelpOptions = { *)
  (*   sighelp_triggerCharacters: string list; (1* wire "triggerCharacters" *1) *)
  (* } *)

  and codeLensOptions = {
    codelens_resolveProvider: bool [@key "resolveProvider"];  (* wire "resolveProvider" *)
  } [@@yojson.allow_extra_fields]

  and documentOnTypeFormattingOptions = {
    firstTriggerCharacter: string;  (* e.g. "}" *)
    moreTriggerCharacter: string list;
  } [@@yojson.allow_extra_fields]

  and documentLinkOptions = {
    doclink_resolveProvider: bool;  (* wire "resolveProvider" *)
  } [@@yojson.allow_extra_fields]

  and executeCommandOptions = {
    commands: string list;  (* the commands to be executed on the server *)
  } [@@yojson.allow_extra_fields]

  (* text document sync options say what messages the server requests the
   * client to send. We use the "want_" prefix for OCaml naming reasons;
   * this prefix is absent in LSP. *)
  and textDocumentSyncOptions = {
    openClose: bool;  (* textDocument/didOpen+didClose *)
    change: textDocumentSyncKind;
    willSave: bool;  (* textDocument/willSave *)
    willSaveWaitUntil: bool;  (* textDoc.../willSaveWaitUntil *)
    didSave: saveOptions option [@default None];  (* textDocument/didSave *)
  } [@@yojson.allow_extra_fields]

  and saveOptions = {
    includeText: bool;  (* the client should include content on save *)
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
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
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let processId_field = ref None
       and rootPath_field = ref None
       and rootUri_field = ref None
       and client_capabilities_field = ref None
       and trace_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "processId" ->
                   (match Ppx_yojson_conv_lib.(!) processId_field with
                    | None ->
                        let fvalue =
                          option_of_yojson int_of_yojson _field_yojson in
                        processId_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "rootPath" ->
                   (match Ppx_yojson_conv_lib.(!) rootPath_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        rootPath_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "rootUri" ->
                   (match Ppx_yojson_conv_lib.(!) rootUri_field with
                    | None ->
                        let fvalue =
                          option_of_yojson documentUri_of_yojson
                            _field_yojson in
                        rootUri_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "capabilities" ->
                   (match Ppx_yojson_conv_lib.(!) client_capabilities_field
                    with
                    | None ->
                        let fvalue =
                          client_capabilities_of_yojson _field_yojson in
                        client_capabilities_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "trace" ->
                   (match Ppx_yojson_conv_lib.(!) trace_field with
                    | None ->
                        let fvalue = trace_of_yojson _field_yojson in
                        trace_field := (Some fvalue)
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
                  let (processId_value, rootPath_value, rootUri_value,
                       client_capabilities_value, trace_value)
                    =
                    ((Ppx_yojson_conv_lib.(!) processId_field),
                      (Ppx_yojson_conv_lib.(!) rootPath_field),
                      (Ppx_yojson_conv_lib.(!) rootUri_field),
                      (Ppx_yojson_conv_lib.(!) client_capabilities_field),
                      (Ppx_yojson_conv_lib.(!) trace_field)) in
                  {
                    processId =
                      ((match processId_value with
                        | None -> None
                        | Some v -> v));
                    rootPath =
                      ((match rootPath_value with
                        | None -> None
                        | Some v -> v));
                    rootUri =
                      ((match rootUri_value with | None -> None | Some v -> v));
                    client_capabilities =
                      ((match client_capabilities_value with
                        | None -> client_capabilities_empty
                        | Some v -> v));
                    trace =
                      ((match trace_value with | None -> Off | Some v -> v))
                  })))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.result" in
   function
   | `Assoc field_yojsons as yojson ->
       let server_capabilities_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "capabilities" ->
                   (match Ppx_yojson_conv_lib.(!) server_capabilities_field
                    with
                    | None ->
                        let fvalue =
                          server_capabilities_of_yojson _field_yojson in
                        server_capabilities_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) server_capabilities_field
                   with
                   | Some server_capabilities_value ->
                       { server_capabilities = server_capabilities_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!)
                                 server_capabilities_field) None),
                            "server_capabilities")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> result)
and errorData_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.errorData" in
   function
   | `Assoc field_yojsons as yojson ->
       let retry_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "retry" ->
                   (match Ppx_yojson_conv_lib.(!) retry_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        retry_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) retry_field with
                   | Some retry_value -> { retry = retry_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) retry_field) None),
                            "retry")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> errorData)
and server_capabilities_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.server_capabilities" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocumentSync_field = ref None
       and hoverProvider_field = ref None
       and completionProvider_field = ref None
       and definitionProvider_field = ref None
       and typeDefinitionProvider_field = ref None
       and referencesProvider_field = ref None
       and documentHighlightProvider_field = ref None
       and documentSymbolProvider_field = ref None
       and workspaceSymbolProvider_field = ref None
       and codeActionProvider_field = ref None
       and codeLensProvider_field = ref None
       and documentFormattingProvider_field = ref None
       and documentRangeFormattingProvider_field = ref None
       and documentOnTypeFormattingProvider_field = ref None
       and renameProvider_field = ref None
       and documentLinkProvider_field = ref None
       and executeCommandProvider_field = ref None
       and typeCoverageProvider_field = ref None
       and rageProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocumentSync" ->
                   (match Ppx_yojson_conv_lib.(!) textDocumentSync_field with
                    | None ->
                        let fvalue =
                          textDocumentSyncOptions_of_yojson _field_yojson in
                        textDocumentSync_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "hoverProvider" ->
                   (match Ppx_yojson_conv_lib.(!) hoverProvider_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        hoverProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "completionProvider" ->
                   (match Ppx_yojson_conv_lib.(!) completionProvider_field
                    with
                    | None ->
                        let fvalue =
                          option_of_yojson completionOptions_of_yojson
                            _field_yojson in
                        completionProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "definitionProvider" ->
                   (match Ppx_yojson_conv_lib.(!) definitionProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        definitionProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "typeDefinitionProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            typeDefinitionProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        typeDefinitionProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "referencesProvider" ->
                   (match Ppx_yojson_conv_lib.(!) referencesProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        referencesProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentHighlightProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            documentHighlightProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        documentHighlightProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentSymbolProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            documentSymbolProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        documentSymbolProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "workspaceSymbolProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            workspaceSymbolProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        workspaceSymbolProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "codeActionProvider" ->
                   (match Ppx_yojson_conv_lib.(!) codeActionProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        codeActionProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "codeLensProvider" ->
                   (match Ppx_yojson_conv_lib.(!) codeLensProvider_field with
                    | None ->
                        let fvalue =
                          option_of_yojson codeLensOptions_of_yojson
                            _field_yojson in
                        codeLensProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentFormattingProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            documentFormattingProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        documentFormattingProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentRangeFormattingProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            documentRangeFormattingProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        documentRangeFormattingProvider_field :=
                          (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentOnTypeFormattingProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            documentOnTypeFormattingProvider_field
                    with
                    | None ->
                        let fvalue =
                          option_of_yojson
                            documentOnTypeFormattingOptions_of_yojson
                            _field_yojson in
                        documentOnTypeFormattingProvider_field :=
                          (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "renameProvider" ->
                   (match Ppx_yojson_conv_lib.(!) renameProvider_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        renameProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "documentLinkProvider" ->
                   (match Ppx_yojson_conv_lib.(!) documentLinkProvider_field
                    with
                    | None ->
                        let fvalue =
                          option_of_yojson documentLinkOptions_of_yojson
                            _field_yojson in
                        documentLinkProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "executeCommandProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            executeCommandProvider_field
                    with
                    | None ->
                        let fvalue =
                          option_of_yojson executeCommandOptions_of_yojson
                            _field_yojson in
                        executeCommandProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "typeCoverageProvider" ->
                   (match Ppx_yojson_conv_lib.(!) typeCoverageProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        typeCoverageProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "rageProvider" ->
                   (match Ppx_yojson_conv_lib.(!) rageProvider_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        rageProvider_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocumentSync_field),
                           (Ppx_yojson_conv_lib.(!) hoverProvider_field),
                           (Ppx_yojson_conv_lib.(!) completionProvider_field),
                           (Ppx_yojson_conv_lib.(!) definitionProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              typeDefinitionProvider_field),
                           (Ppx_yojson_conv_lib.(!) referencesProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentHighlightProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentSymbolProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              workspaceSymbolProvider_field),
                           (Ppx_yojson_conv_lib.(!) codeActionProvider_field),
                           (Ppx_yojson_conv_lib.(!) codeLensProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentFormattingProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentRangeFormattingProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentOnTypeFormattingProvider_field),
                           (Ppx_yojson_conv_lib.(!) renameProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              documentLinkProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              executeCommandProvider_field),
                           (Ppx_yojson_conv_lib.(!)
                              typeCoverageProvider_field),
                           (Ppx_yojson_conv_lib.(!) rageProvider_field))
                   with
                   | (Some textDocumentSync_value, Some hoverProvider_value,
                      completionProvider_value, Some
                      definitionProvider_value, Some
                      typeDefinitionProvider_value, Some
                      referencesProvider_value, Some
                      documentHighlightProvider_value, Some
                      documentSymbolProvider_value, Some
                      workspaceSymbolProvider_value, Some
                      codeActionProvider_value, codeLensProvider_value, Some
                      documentFormattingProvider_value, Some
                      documentRangeFormattingProvider_value,
                      documentOnTypeFormattingProvider_value, Some
                      renameProvider_value, documentLinkProvider_value,
                      executeCommandProvider_value, Some
                      typeCoverageProvider_value, Some rageProvider_value) ->
                       {
                         textDocumentSync = textDocumentSync_value;
                         hoverProvider = hoverProvider_value;
                         completionProvider =
                           ((match completionProvider_value with
                             | None -> None
                             | Some v -> v));
                         definitionProvider = definitionProvider_value;
                         typeDefinitionProvider =
                           typeDefinitionProvider_value;
                         referencesProvider = referencesProvider_value;
                         documentHighlightProvider =
                           documentHighlightProvider_value;
                         documentSymbolProvider =
                           documentSymbolProvider_value;
                         workspaceSymbolProvider =
                           workspaceSymbolProvider_value;
                         codeActionProvider = codeActionProvider_value;
                         codeLensProvider =
                           ((match codeLensProvider_value with
                             | None -> None
                             | Some v -> v));
                         documentFormattingProvider =
                           documentFormattingProvider_value;
                         documentRangeFormattingProvider =
                           documentRangeFormattingProvider_value;
                         documentOnTypeFormattingProvider =
                           ((match documentOnTypeFormattingProvider_value
                             with
                             | None -> None
                             | Some v -> v));
                         renameProvider = renameProvider_value;
                         documentLinkProvider =
                           ((match documentLinkProvider_value with
                             | None -> None
                             | Some v -> v));
                         executeCommandProvider =
                           ((match executeCommandProvider_value with
                             | None -> None
                             | Some v -> v));
                         typeCoverageProvider = typeCoverageProvider_value;
                         rageProvider = rageProvider_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocumentSync_field)
                              None), "textDocumentSync");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) hoverProvider_field)
                             None), "hoverProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                definitionProvider_field) None),
                           "definitionProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                typeDefinitionProvider_field) None),
                           "typeDefinitionProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                referencesProvider_field) None),
                           "referencesProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                documentHighlightProvider_field) None),
                           "documentHighlightProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                documentSymbolProvider_field) None),
                           "documentSymbolProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                workspaceSymbolProvider_field) None),
                           "workspaceSymbolProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                codeActionProvider_field) None),
                           "codeActionProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                documentFormattingProvider_field) None),
                           "documentFormattingProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                documentRangeFormattingProvider_field) None),
                           "documentRangeFormattingProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) renameProvider_field)
                             None), "renameProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                typeCoverageProvider_field) None),
                           "typeCoverageProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) rageProvider_field)
                             None), "rageProvider")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> server_capabilities)
and completionOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.completionOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let resolveProvider_field = ref None
       and triggerCharacters_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "resolveProvider" ->
                   (match Ppx_yojson_conv_lib.(!) resolveProvider_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        resolveProvider_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "triggerCharacters" ->
                   (match Ppx_yojson_conv_lib.(!) triggerCharacters_field
                    with
                    | None ->
                        let fvalue =
                          list_of_yojson string_of_yojson _field_yojson in
                        triggerCharacters_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) resolveProvider_field),
                           (Ppx_yojson_conv_lib.(!) triggerCharacters_field))
                   with
                   | (Some resolveProvider_value, Some
                      triggerCharacters_value) ->
                       {
                         resolveProvider = resolveProvider_value;
                         triggerCharacters = triggerCharacters_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) resolveProvider_field)
                              None), "resolveProvider");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) triggerCharacters_field)
                             None), "triggerCharacters")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> completionOptions)
and codeLensOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.codeLensOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let codelens_resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "resolveProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            codelens_resolveProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        codelens_resolveProvider_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!)
                           codelens_resolveProvider_field
                   with
                   | Some codelens_resolveProvider_value ->
                       {
                         codelens_resolveProvider =
                           codelens_resolveProvider_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!)
                                 codelens_resolveProvider_field) None),
                            "codelens_resolveProvider")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> codeLensOptions)
and documentOnTypeFormattingOptions_of_yojson =
  (let _tp_loc =
     "src/lsp/protocol.ml.Initialize.documentOnTypeFormattingOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let firstTriggerCharacter_field = ref None
       and moreTriggerCharacter_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "firstTriggerCharacter" ->
                   (match Ppx_yojson_conv_lib.(!) firstTriggerCharacter_field
                    with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        firstTriggerCharacter_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "moreTriggerCharacter" ->
                   (match Ppx_yojson_conv_lib.(!) moreTriggerCharacter_field
                    with
                    | None ->
                        let fvalue =
                          list_of_yojson string_of_yojson _field_yojson in
                        moreTriggerCharacter_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!)
                             firstTriggerCharacter_field),
                           (Ppx_yojson_conv_lib.(!)
                              moreTriggerCharacter_field))
                   with
                   | (Some firstTriggerCharacter_value, Some
                      moreTriggerCharacter_value) ->
                       {
                         firstTriggerCharacter = firstTriggerCharacter_value;
                         moreTriggerCharacter = moreTriggerCharacter_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!)
                                 firstTriggerCharacter_field) None),
                            "firstTriggerCharacter");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!)
                                moreTriggerCharacter_field) None),
                           "moreTriggerCharacter")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    documentOnTypeFormattingOptions)
and documentLinkOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.documentLinkOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let doclink_resolveProvider_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "doclink_resolveProvider" ->
                   (match Ppx_yojson_conv_lib.(!)
                            doclink_resolveProvider_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        doclink_resolveProvider_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!)
                           doclink_resolveProvider_field
                   with
                   | Some doclink_resolveProvider_value ->
                       {
                         doclink_resolveProvider =
                           doclink_resolveProvider_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!)
                                 doclink_resolveProvider_field) None),
                            "doclink_resolveProvider")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> documentLinkOptions)
and executeCommandOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.executeCommandOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let commands_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "commands" ->
                   (match Ppx_yojson_conv_lib.(!) commands_field with
                    | None ->
                        let fvalue =
                          list_of_yojson string_of_yojson _field_yojson in
                        commands_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) commands_field with
                   | Some commands_value -> { commands = commands_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) commands_field) None),
                            "commands")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> executeCommandOptions)
and textDocumentSyncOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.textDocumentSyncOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let openClose_field = ref None
       and change_field = ref None
       and willSave_field = ref None
       and willSaveWaitUntil_field = ref None
       and didSave_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "openClose" ->
                   (match Ppx_yojson_conv_lib.(!) openClose_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        openClose_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "change" ->
                   (match Ppx_yojson_conv_lib.(!) change_field with
                    | None ->
                        let fvalue =
                          textDocumentSyncKind_of_yojson _field_yojson in
                        change_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "willSave" ->
                   (match Ppx_yojson_conv_lib.(!) willSave_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        willSave_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "willSaveWaitUntil" ->
                   (match Ppx_yojson_conv_lib.(!) willSaveWaitUntil_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        willSaveWaitUntil_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "didSave" ->
                   (match Ppx_yojson_conv_lib.(!) didSave_field with
                    | None ->
                        let fvalue =
                          option_of_yojson saveOptions_of_yojson
                            _field_yojson in
                        didSave_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) openClose_field),
                           (Ppx_yojson_conv_lib.(!) change_field),
                           (Ppx_yojson_conv_lib.(!) willSave_field),
                           (Ppx_yojson_conv_lib.(!) willSaveWaitUntil_field),
                           (Ppx_yojson_conv_lib.(!) didSave_field))
                   with
                   | (Some openClose_value, Some change_value, Some
                      willSave_value, Some willSaveWaitUntil_value,
                      didSave_value) ->
                       {
                         openClose = openClose_value;
                         change = change_value;
                         willSave = willSave_value;
                         willSaveWaitUntil = willSaveWaitUntil_value;
                         didSave =
                           ((match didSave_value with
                             | None -> None
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) openClose_field) None),
                            "openClose");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) change_field) None),
                           "change");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) willSave_field) None),
                           "willSave");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) willSaveWaitUntil_field)
                             None), "willSaveWaitUntil")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t ->
                    textDocumentSyncOptions)
and saveOptions_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Initialize.saveOptions" in
   function
   | `Assoc field_yojsons as yojson ->
       let includeText_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "includeText" ->
                   (match Ppx_yojson_conv_lib.(!) includeText_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        includeText_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) includeText_field with
                   | Some includeText_value ->
                       { includeText = includeText_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) includeText_field)
                              None), "includeText")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> saveOptions)
let _ = params_of_yojson
and _ = result_of_yojson
and _ = errorData_of_yojson
and _ = server_capabilities_of_yojson
and _ = completionOptions_of_yojson
and _ = codeLensOptions_of_yojson
and _ = documentOnTypeFormattingOptions_of_yojson
and _ = documentLinkOptions_of_yojson
and _ = executeCommandOptions_of_yojson
and _ = textDocumentSyncOptions_of_yojson
and _ = saveOptions_of_yojson
let rec yojson_of_params =
  (function
   | { processId = v_processId; rootPath = v_rootPath; rootUri = v_rootUri;
       client_capabilities = v_client_capabilities; trace = v_trace } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_trace v_trace in ("trace", arg) :: bnds in
       let bnds =
         let arg = yojson_of_client_capabilities v_client_capabilities in
         ("capabilities", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_documentUri v_rootUri in
         ("rootUri", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_rootPath in
         ("rootPath", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_int v_processId in
         ("processId", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (function
   | { server_capabilities = v_server_capabilities } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_server_capabilities v_server_capabilities in
         ("capabilities", arg) :: bnds in
       `Assoc bnds : result -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_errorData =
  (function
   | { retry = v_retry } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_bool v_retry in ("retry", arg) :: bnds in
       `Assoc bnds : errorData -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_server_capabilities =
  (function
   | { textDocumentSync = v_textDocumentSync;
       hoverProvider = v_hoverProvider;
       completionProvider = v_completionProvider;
       definitionProvider = v_definitionProvider;
       typeDefinitionProvider = v_typeDefinitionProvider;
       referencesProvider = v_referencesProvider;
       documentHighlightProvider = v_documentHighlightProvider;
       documentSymbolProvider = v_documentSymbolProvider;
       workspaceSymbolProvider = v_workspaceSymbolProvider;
       codeActionProvider = v_codeActionProvider;
       codeLensProvider = v_codeLensProvider;
       documentFormattingProvider = v_documentFormattingProvider;
       documentRangeFormattingProvider = v_documentRangeFormattingProvider;
       documentOnTypeFormattingProvider = v_documentOnTypeFormattingProvider;
       renameProvider = v_renameProvider;
       documentLinkProvider = v_documentLinkProvider;
       executeCommandProvider = v_executeCommandProvider;
       typeCoverageProvider = v_typeCoverageProvider;
       rageProvider = v_rageProvider } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_rageProvider in ("rageProvider", arg) ::
           bnds in
       let bnds =
         let arg = yojson_of_bool v_typeCoverageProvider in
         ("typeCoverageProvider", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_executeCommandOptions
             v_executeCommandProvider in
         ("executeCommandProvider", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_documentLinkOptions
             v_documentLinkProvider in
         ("documentLinkProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_renameProvider in ("renameProvider", arg)
           :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_documentOnTypeFormattingOptions
             v_documentOnTypeFormattingProvider in
         ("documentOnTypeFormattingProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_documentRangeFormattingProvider in
         ("documentRangeFormattingProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_documentFormattingProvider in
         ("documentFormattingProvider", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_codeLensOptions v_codeLensProvider in
         ("codeLensProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_codeActionProvider in
         ("codeActionProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_workspaceSymbolProvider in
         ("workspaceSymbolProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_documentSymbolProvider in
         ("documentSymbolProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_documentHighlightProvider in
         ("documentHighlightProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_referencesProvider in
         ("referencesProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_typeDefinitionProvider in
         ("typeDefinitionProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_definitionProvider in
         ("definitionProvider", arg) :: bnds in
       let bnds =
         let arg =
           yojson_of_option yojson_of_completionOptions v_completionProvider in
         ("completionProvider", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_hoverProvider in ("hoverProvider", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_textDocumentSyncOptions v_textDocumentSync in
         ("textDocumentSync", arg) :: bnds in
       `Assoc bnds : server_capabilities -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_completionOptions =
  (function
   | { resolveProvider = v_resolveProvider;
       triggerCharacters = v_triggerCharacters } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_string v_triggerCharacters in
         ("triggerCharacters", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_resolveProvider in
         ("resolveProvider", arg) :: bnds in
       `Assoc bnds : completionOptions -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_codeLensOptions =
  (function
   | { codelens_resolveProvider = v_codelens_resolveProvider } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_codelens_resolveProvider in
         ("resolveProvider", arg) :: bnds in
       `Assoc bnds : codeLensOptions -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_documentOnTypeFormattingOptions =
  (function
   | { firstTriggerCharacter = v_firstTriggerCharacter;
       moreTriggerCharacter = v_moreTriggerCharacter } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_string v_moreTriggerCharacter in
         ("moreTriggerCharacter", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_firstTriggerCharacter in
         ("firstTriggerCharacter", arg) :: bnds in
       `Assoc bnds : documentOnTypeFormattingOptions ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_documentLinkOptions =
  (function
   | { doclink_resolveProvider = v_doclink_resolveProvider } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_doclink_resolveProvider in
         ("doclink_resolveProvider", arg) :: bnds in
       `Assoc bnds : documentLinkOptions -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_executeCommandOptions =
  (function
   | { commands = v_commands } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_string v_commands in
         ("commands", arg) :: bnds in
       `Assoc bnds : executeCommandOptions ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_textDocumentSyncOptions =
  (function
   | { openClose = v_openClose; change = v_change; willSave = v_willSave;
       willSaveWaitUntil = v_willSaveWaitUntil; didSave = v_didSave } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_saveOptions v_didSave in
         ("didSave", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_willSaveWaitUntil in
         ("willSaveWaitUntil", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_willSave in ("willSave", arg) :: bnds in
       let bnds =
         let arg = yojson_of_textDocumentSyncKind v_change in ("change", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_bool v_openClose in ("openClose", arg) :: bnds in
       `Assoc bnds : textDocumentSyncOptions ->
                       Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_saveOptions =
  (function
   | { includeText = v_includeText } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_includeText in ("includeText", arg) ::
           bnds in
       `Assoc bnds : saveOptions -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
and _ = yojson_of_errorData
and _ = yojson_of_server_capabilities
and _ = yojson_of_completionOptions
and _ = yojson_of_codeLensOptions
and _ = yojson_of_documentOnTypeFormattingOptions
and _ = yojson_of_documentLinkOptions
and _ = yojson_of_executeCommandOptions
and _ = yojson_of_textDocumentSyncOptions
and _ = yojson_of_saveOptions
[@@@end]
end

(* Goto Definition request, method="textDocument/definition" *)
module Definition = struct [@@@ocaml.warning "-39"]
  type params = TextDocumentPositionParams.t

  and result = DefinitionLocation.t list  (* wire: either a single one or an array *)
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Definition.params" in
   fun t -> TextDocumentPositionParams.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> params) 
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Definition.result" in
   fun t -> list_of_yojson DefinitionLocation.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                                -> result) 

let _ = params_of_yojson
and _ = result_of_yojson
let yojson_of_params =
  (TextDocumentPositionParams.yojson_of_t : params ->
                                              Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_list DefinitionLocation.yojson_of_t v : result ->
                                                                Ppx_yojson_conv_lib.Yojson.Safe.t) 
let _ = yojson_of_params
and _ = yojson_of_result
[@@@end]
end 

(* Goto Type Definition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct [@@@ocaml.warning "-39"]
  type params = TextDocumentPositionParams.t

  and result = Location.t list  (* wire: either a single one or an array *)
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TypeDefinition.params" in
   fun t -> TextDocumentPositionParams.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TypeDefinition.result" in
   fun t -> list_of_yojson Location.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                      -> result)
let _ = params_of_yojson
and _ = result_of_yojson
let yojson_of_params =
  (TextDocumentPositionParams.yojson_of_t : params ->
                                              Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_list Location.yojson_of_t v : result ->
                                                      Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
[@@@end]
end

(* References request, method="textDocument/references" *)
module References = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: referenceContext;
  } [@@yojson.allow_extra_fields]

  and referenceContext = {
    includeDeclaration: bool;
  } [@@yojson.allow_extra_fields]

  and result = Location.t list (* wire: either a single one or an array *)
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : referenceContext) -> ()
let _ = fun (_ : result) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.References.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and position_field = ref None
       and context_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "position" ->
                   (match Ppx_yojson_conv_lib.(!) position_field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        position_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "context" ->
                   (match Ppx_yojson_conv_lib.(!) context_field with
                    | None ->
                        let fvalue = referenceContext_of_yojson _field_yojson in
                        context_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) position_field),
                           (Ppx_yojson_conv_lib.(!) context_field))
                   with
                   | (Some textDocument_value, Some position_value, Some
                      context_value) ->
                       {
                         textDocument = textDocument_value;
                         position = position_value;
                         context = context_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) position_field) None),
                           "position");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) context_field) None),
                           "context")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
and referenceContext_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.References.referenceContext" in
   function
   | `Assoc field_yojsons as yojson ->
       let includeDeclaration_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "includeDeclaration" ->
                   (match Ppx_yojson_conv_lib.(!) includeDeclaration_field
                    with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        includeDeclaration_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) includeDeclaration_field
                   with
                   | Some includeDeclaration_value ->
                       { includeDeclaration = includeDeclaration_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!)
                                 includeDeclaration_field) None),
                            "includeDeclaration")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> referenceContext)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.References.result" in
   fun t -> list_of_yojson Location.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                      -> result)
let _ = params_of_yojson
and _ = referenceContext_of_yojson
and _ = result_of_yojson
let rec yojson_of_params =
  (function
   | { textDocument = v_textDocument; position = v_position;
       context = v_context } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_referenceContext v_context in ("context", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_position v_position in ("position", arg) :: bnds in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_referenceContext =
  (function
   | { includeDeclaration = v_includeDeclaration } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_bool v_includeDeclaration in
         ("includeDeclaration", arg) :: bnds in
       `Assoc bnds : referenceContext -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_list Location.yojson_of_t v : result ->
                                                      Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_referenceContext
and _ = yojson_of_result
[@@@end]
end

(* DocumentHighlight request, method="textDocument/documentHighlight" *)
module TextDocumentHighlight = struct [@@@ocaml.warning "-39"]
  type params = TextDocumentPositionParams.t

  and result = DocumentHighlight.t list (* wire: either a single one or an array *)
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentHighlight.params" in
   fun t -> TextDocumentPositionParams.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                         -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentHighlight.result" in
   fun t -> list_of_yojson DocumentHighlight.t_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                               -> result)
let _ = params_of_yojson
and _ = result_of_yojson
let yojson_of_params =
  (TextDocumentPositionParams.yojson_of_t : params ->
                                              Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_list DocumentHighlight.yojson_of_t v : result ->
                                                               Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
[@@@end]
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

  let yojson_of_t = function
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

  let t_of_yojson = function
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
    | node -> yojson_error "invalid SymbolKind" node

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
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : t) -> ()
let t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.SymbolInformation.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let name_field = ref None
       and kind_field = ref None
       and deprecated_field = ref None
       and location_field = ref None
       and containerName_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "name" ->
                   (match Ppx_yojson_conv_lib.(!) name_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        name_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "kind" ->
                   (match Ppx_yojson_conv_lib.(!) kind_field with
                    | None ->
                        let fvalue = SymbolKind.t_of_yojson _field_yojson in
                        kind_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "deprecated" ->
                   (match Ppx_yojson_conv_lib.(!) deprecated_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        deprecated_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "location" ->
                   (match Ppx_yojson_conv_lib.(!) location_field with
                    | None ->
                        let fvalue = Location.t_of_yojson _field_yojson in
                        location_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "containerName" ->
                   (match Ppx_yojson_conv_lib.(!) containerName_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        containerName_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) name_field),
                           (Ppx_yojson_conv_lib.(!) kind_field),
                           (Ppx_yojson_conv_lib.(!) deprecated_field),
                           (Ppx_yojson_conv_lib.(!) location_field),
                           (Ppx_yojson_conv_lib.(!) containerName_field))
                   with
                   | (Some name_value, Some kind_value, deprecated_value,
                      Some location_value, containerName_value) ->
                       {
                         name = name_value;
                         kind = kind_value;
                         deprecated =
                           ((match deprecated_value with
                             | None -> false
                             | Some v -> v));
                         location = location_value;
                         containerName =
                           ((match containerName_value with
                             | None -> None
                             | Some v -> v))
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) name_field) None),
                            "name");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) kind_field) None),
                           "kind");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) location_field) None),
                           "location")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let yojson_of_t =
  (function
   | { name = v_name; kind = v_kind; deprecated = v_deprecated;
       location = v_location; containerName = v_containerName } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_containerName in
         ("containerName", arg) :: bnds in
       let bnds =
         let arg = Location.yojson_of_t v_location in ("location", arg) ::
           bnds in
       let bnds =
         let arg = yojson_of_bool v_deprecated in ("deprecated", arg) :: bnds in
       let bnds =
         let arg = SymbolKind.yojson_of_t v_kind in ("kind", arg) :: bnds in
       let bnds = let arg = yojson_of_string v_name in ("name", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

module DocumentSymbol = struct

  type t = {
    name : string;
    (**
     * The name of this symbol. Will be displayed in the user interface and
     * therefore must not be an empty string or a string only consisting of
     * white spaces.
     *)

    detail: string option;
    (**
     * More detail for this symbol, e.g the signature of a function.
     *)

    kind: SymbolKind.t;
    (**
     * The kind of this symbol.
     *)

    deprecated : bool;
    (**
     * Indicates if this symbol is deprecated.
     *)

    range : range;
    (**
     * The range enclosing this symbol not including leading/trailing whitespace
     * but everything else like comments. This information is typically used to
     * determine if the clients cursor is inside the symbol to reveal in the
     * symbol in the UI.
     *)

    selectionRange : range;
    (**
     * The range that should be selected and revealed when this symbol is being
     * picked, e.g the name of a function.  Must be contained by the `range`.
     *)

    children: t list;
    (**
     * Children of this symbol, e.g. properties of a class.
     *)
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : t) -> ()
let rec t_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DocumentSymbol.t" in
   function
   | `Assoc field_yojsons as yojson ->
       let name_field = ref None
       and detail_field = ref None
       and kind_field = ref None
       and deprecated_field = ref None
       and range_field = ref None
       and selectionRange_field = ref None
       and children_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "name" ->
                   (match Ppx_yojson_conv_lib.(!) name_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        name_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "detail" ->
                   (match Ppx_yojson_conv_lib.(!) detail_field with
                    | None ->
                        let fvalue =
                          option_of_yojson string_of_yojson _field_yojson in
                        detail_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "kind" ->
                   (match Ppx_yojson_conv_lib.(!) kind_field with
                    | None ->
                        let fvalue = SymbolKind.t_of_yojson _field_yojson in
                        kind_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "deprecated" ->
                   (match Ppx_yojson_conv_lib.(!) deprecated_field with
                    | None ->
                        let fvalue = bool_of_yojson _field_yojson in
                        deprecated_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "selectionRange" ->
                   (match Ppx_yojson_conv_lib.(!) selectionRange_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        selectionRange_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "children" ->
                   (match Ppx_yojson_conv_lib.(!) children_field with
                    | None ->
                        let fvalue = list_of_yojson t_of_yojson _field_yojson in
                        children_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) name_field),
                           (Ppx_yojson_conv_lib.(!) detail_field),
                           (Ppx_yojson_conv_lib.(!) kind_field),
                           (Ppx_yojson_conv_lib.(!) deprecated_field),
                           (Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) selectionRange_field),
                           (Ppx_yojson_conv_lib.(!) children_field))
                   with
                   | (Some name_value, Some detail_value, Some kind_value,
                      Some deprecated_value, Some range_value, Some
                      selectionRange_value, Some children_value) ->
                       {
                         name = name_value;
                         detail = detail_value;
                         kind = kind_value;
                         deprecated = deprecated_value;
                         range = range_value;
                         selectionRange = selectionRange_value;
                         children = children_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) name_field) None),
                            "name");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) detail_field) None),
                           "detail");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) kind_field) None),
                           "kind");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) deprecated_field) None),
                           "deprecated");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) range_field) None),
                           "range");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) selectionRange_field)
                             None), "selectionRange");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) children_field) None),
                           "children")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
let _ = t_of_yojson
let rec yojson_of_t =
  (function
   | { name = v_name; detail = v_detail; kind = v_kind;
       deprecated = v_deprecated; range = v_range;
       selectionRange = v_selectionRange; children = v_children } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_list yojson_of_t v_children in ("children", arg)
           :: bnds in
       let bnds =
         let arg = yojson_of_range v_selectionRange in
         ("selectionRange", arg) :: bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       let bnds =
         let arg = yojson_of_bool v_deprecated in ("deprecated", arg) :: bnds in
       let bnds =
         let arg = SymbolKind.yojson_of_t v_kind in ("kind", arg) :: bnds in
       let bnds =
         let arg = yojson_of_option yojson_of_string v_detail in
         ("detail", arg) :: bnds in
       let bnds = let arg = yojson_of_string v_name in ("name", arg) :: bnds in
       `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_t
[@@@end]
end

(* Document Symbols request, method="textDocument/documentSymbols" *)
module TextDocumentDocumentSymbol = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.TextDocumentDocumentSymbol.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) textDocument_field with
                   | Some textDocument_value ->
                       { textDocument = textDocument_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
let _ = params_of_yojson
let yojson_of_params =
  (function
   | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
[@@@end]

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  let yojson_of_result = function
    | DocumentSymbol symbols ->
      `List (Std.List.map symbols ~f:DocumentSymbol.yojson_of_t)
    | SymbolInformation symbols ->
      `List (Std.List.map symbols ~f:SymbolInformation.yojson_of_t)

end

module CodeLens = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
  } [@@yojson.allow_extra_fields]

  and result = item list

  and item = {
    range: range;
    command: Command.t option;
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
   
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let _ = fun (_ : item) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.CodeLens.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) textDocument_field with
                   | Some textDocument_value ->
                       { textDocument = textDocument_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.CodeLens.result" in
   fun t -> list_of_yojson item_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t
                                                -> result)
and item_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.CodeLens.item" in
   function
   | `Assoc field_yojsons as yojson ->
       let range_field = ref None
       and command_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "command" ->
                   (match Ppx_yojson_conv_lib.(!) command_field with
                    | None ->
                        let fvalue =
                          option_of_yojson Command.t_of_yojson _field_yojson in
                        command_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) range_field),
                           (Ppx_yojson_conv_lib.(!) command_field))
                   with
                   | (Some range_value, Some command_value) ->
                       { range = range_value; command = command_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) range_field) None),
                            "range");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) command_field) None),
                           "command")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> item)
let _ = params_of_yojson
and _ = result_of_yojson
and _ = item_of_yojson
let rec yojson_of_params =
  (function
   | { textDocument = v_textDocument } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_list yojson_of_item v : result ->
                                                Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_item =
  (function
   | { range = v_range; command = v_command } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_option Command.yojson_of_t v_command in
         ("command", arg) :: bnds in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       `Assoc bnds : item -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
and _ = yojson_of_item
[@@@end]
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
  } [@@yojson.allow_extra_fields]
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.Rename.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and position_field = ref None
       and newName_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "position" ->
                   (match Ppx_yojson_conv_lib.(!) position_field with
                    | None ->
                        let fvalue = position_of_yojson _field_yojson in
                        position_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "newName" ->
                   (match Ppx_yojson_conv_lib.(!) newName_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        newName_field := (Some fvalue)
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
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) position_field),
                           (Ppx_yojson_conv_lib.(!) newName_field))
                   with
                   | (Some textDocument_value, Some position_value, Some
                      newName_value) ->
                       {
                         textDocument = textDocument_value;
                         position = position_value;
                         newName = newName_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) position_field) None),
                           "position");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) newName_field) None),
                           "newName")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
let _ = params_of_yojson
let yojson_of_params =
  (function
   | { textDocument = v_textDocument; position = v_position;
       newName = v_newName } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_newName in ("newName", arg) :: bnds in
       let bnds =
         let arg = yojson_of_position v_position in ("position", arg) :: bnds in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
[@@@end]

  type result = WorkspaceEdit.t
  [@@deriving_inline yojson_of]
  
let _ = fun (_ : result) -> ()
let yojson_of_result =
  (WorkspaceEdit.yojson_of_t : result -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_result
[@@@end]
end

module DebugEcho = struct
  type params = {
    message: string;
  } [@@yojson.allow_extra_fields]

  and result = params
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let _ = fun (_ : result) -> ()
let rec params_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DebugEcho.params" in
   function
   | `Assoc field_yojsons as yojson ->
       let message_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "message" ->
                   (match Ppx_yojson_conv_lib.(!) message_field with
                    | None ->
                        let fvalue = string_of_yojson _field_yojson in
                        message_field := (Some fvalue)
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
                  (match Ppx_yojson_conv_lib.(!) message_field with
                   | Some message_value -> { message = message_value }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) message_field) None),
                            "message")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> params)
and result_of_yojson =
  (let _tp_loc = "src/lsp/protocol.ml.DebugEcho.result" in
   fun t -> params_of_yojson t : Ppx_yojson_conv_lib.Yojson.Safe.t -> result)
let _ = params_of_yojson
and _ = result_of_yojson
let rec yojson_of_params =
  (function
   | { message = v_message } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_string v_message in ("message", arg) :: bnds in
       `Assoc bnds : params -> Ppx_yojson_conv_lib.Yojson.Safe.t)
and yojson_of_result =
  (fun v -> yojson_of_params v : result -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
and _ = yojson_of_result
[@@@end]
end

module DebugTextDocumentGet = struct
  type params = TextDocumentPositionParams.t
  [@@deriving_inline yojson]
  
let _ = fun (_ : params) -> ()
let params_of_yojson =
  (TextDocumentPositionParams.t_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t
                                              -> params)
let _ = params_of_yojson
let yojson_of_params =
  (TextDocumentPositionParams.yojson_of_t : params ->
                                              Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_params
[@@@end]

  type result = string option [@default None]

  let yojson_of_result = function
    | Some s -> `String s
    | None -> `Null
end
