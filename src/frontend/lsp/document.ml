let {Logger. log} = Logger.for_section "ocamlmerlin-lsp"

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
let with_pipeline doc f =
  Mpipeline.with_pipeline doc.pipeline (fun () -> f doc.pipeline)
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
