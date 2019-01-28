open Protocol

type t = {
  documentUri : documentUri;
  version : int;
  text : string;
}

let make ?(version=0) documentUri text =
  let text = Text_document_text.normalize_line_endings text in
  {documentUri; version; text;}

let documentUri doc = doc.documentUri
let version doc = doc.version
let text doc = doc.text

let apply_content_change ?version (change : DidChange.textDocumentContentChangeEvent) doc =
  let version =
    match version with
    | None -> doc.version + 1
    | Some version -> version
  in
  let text =
    match change.range with
    | None ->
      Text_document_text.normalize_line_endings change.text
    | Some range ->
      Text_document_text.apply_change doc.text range change.text
  in
  {doc with version; text;}

