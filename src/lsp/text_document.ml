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

