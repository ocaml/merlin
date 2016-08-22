module Typer = struct
  type t = {
    errors : exn list lazy_t;
    result : Mtyper.result;
  }
end

module Ppx = struct
  type t = {
    config : Mconfig.t;
    errors : exn list;
    parsetree : Mreader.parsetree;
  }
end

type t = {
  config : Mconfig.t;
  source : Msource.t;
  reader : (Mreader.result * Mconfig.t) lazy_t;
  ppx    : Ppx.t lazy_t;
  typer  : Typer.t lazy_t;
}

let input_config t = t.config
let input_source t = t.source

let reader t = Lazy.force t.reader
let ppx    t = Lazy.force t.ppx
let typer  t = Lazy.force t.typer

let reader_config    t = (snd (reader t))
let reader_parsetree t = (fst (reader t)).Mreader.parsetree
let reader_comments  t = (fst (reader t)).Mreader.comments
let reader_lexer_errors  t = (fst (reader t)).Mreader.lexer_errors
let reader_parser_errors t = (fst (reader t)).Mreader.parser_errors
let reader_no_labels_for_completion t =
  (fst (reader t)).Mreader.no_labels_for_completion

let ppx_parsetree t = (ppx t).Ppx.parsetree
let ppx_errors    t = (ppx t).Ppx.errors

let final_config  t = (ppx t).Ppx.config

let typer_result t = (typer t).Typer.result
let typer_errors t = Lazy.force (typer t).Typer.errors

let process trace config source reader =
  let ppx = lazy (
    let lazy ({Mreader.parsetree}, config) = reader in
    let caught = ref [] in
    Merlin_support.catch_errors caught @@ fun () ->
    let config, parsetree = Mppx.rewrite trace config parsetree in
    { Ppx. config; parsetree; errors = !caught }
  ) in
  let typer = lazy (
    let lazy { Ppx. config; parsetree; errors } = ppx in
    let result = Mtyper.run config source parsetree in
    let errors = lazy (Mtyper.get_errors result) in
    { Typer. errors; result }
  ) in
  { config; source; reader; ppx; typer }

let make trace config source =
  let config = Mconfig.normalize trace config in
  let reader = lazy (
    let result = Mreader.run trace config source in
    let config = Mconfig.normalize trace config in
    result, config
  ) in
  process trace config source reader

let make_for_completion trace config source position =
  let config = Mconfig.normalize trace config in
  let reader = lazy (
    let result = Mreader.run_for_completion trace config source position in
    let config = Mconfig.normalize trace config in
    result, config
  ) in
  process trace config source reader
