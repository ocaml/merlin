#/bin/sh
REPLAY=./replay
OCAMLFIND=${OCAMLFIND:-ocamlfind}
OCAMLC=${OCAMLC:-ocamlc}
OCAMLOPT=${OCAMLOPT:-ocamlopt}
OCAMLMKTOP=${OCAMLMKTOP:-ocamlmktop}
OCAMLCP=${OCAMLCP:-ocamlcp}
OCAMLDEP=${OCAMLDEP:-ocamldep}

OCAMLFLAGS="-g -bin-annot -w -3"
TYPER=src/ocaml/typer
PACKAGES="str findlib yojson unix merlin_extend"

map()
{
  FN=$1
  shift 1
  while [ $# -gt 0 ]; do
    eval $FN $1
    shift 1
  done
}

intersperse()
{
  SEP=$1
  shift 1
  while [ $# -gt 0 ]; do
    echo $SEP $1
    shift 1
  done
}

ocamlfind_flags()
{
  OCAMLFIND_COMMANDS="ocamlc=echo" $OCAMLFIND c "$@"
}

SOURCES_LIB="
	src/config/my_config.ml
	src/platform/fs_case.c
	src/sturgeon_null/sturgeon_stub.ml
	src/utils/menhirLib.mli
	src/utils/menhirLib.ml
	${TYPER}/printf_compat.mli
	${TYPER}/printf_compat.ml
	src/utils/trace.mli
	src/utils/trace.ml
	src/utils/std.ml
	src/utils/misc.mli
	src/utils/misc.ml
	src/utils/sexp.mli
	src/utils/sexp.ml
	src/utils/file_cache.mli
	src/utils/file_cache.ml
	src/utils/logger.mli
	src/utils/logger.ml
	src/utils/ppxsetup.mli
	src/utils/ppxsetup.ml
	src/ocaml/support/identifiable.mli
	src/ocaml/support/identifiable.ml
	src/ocaml/support/tbl.mli
	src/ocaml/support/tbl.ml
	${TYPER}/utils/warnings.mli
	${TYPER}/utils/warnings.ml
	${TYPER}/utils/config.mli
	${TYPER}/utils/config.ml
	src/ocaml/support/clflags.mli
	src/ocaml/support/clflags.ml
	${TYPER}/parsing/asttypes.mli
	${TYPER}/parsing/location.mli
	${TYPER}/parsing/location.ml
	src/ocaml/support/location_aux.mli
	src/ocaml/support/location_aux.ml
	${TYPER}/parsing/parsetree.mli
	${TYPER}/parsing/attr_helper.mli
	${TYPER}/parsing/attr_helper.ml
	${TYPER}/parsing/ast_iterator.mli
	${TYPER}/parsing/ast_iterator.ml
	${TYPER}/parsing/builtin_attributes.mli
	${TYPER}/parsing/builtin_attributes.ml
	${TYPER}/parsing/longident.mli
	${TYPER}/parsing/longident.ml
	${TYPER}/parsing/docstrings.mli
	${TYPER}/parsing/docstrings.ml
	${TYPER}/parsing/ast_helper.mli
	${TYPER}/parsing/ast_helper.ml
	${TYPER}/parsing/ast_mapper.mli
	${TYPER}/parsing/ast_mapper.ml
	${TYPER}/parsing/printast.mli
	${TYPER}/parsing/printast.ml
	${TYPER}/parsing/pprintast.mli
	${TYPER}/parsing/pprintast.ml
	${TYPER}/typing/annot.mli
	${TYPER}/typing/outcometree.mli
	${TYPER}/typing/ident.mli
	${TYPER}/typing/ident.ml
	${TYPER}/typing/path.mli
	${TYPER}/typing/path.ml
	${TYPER}/typing/primitive.mli
	${TYPER}/typing/primitive.ml
	${TYPER}/typing/types.mli
	${TYPER}/typing/types.ml
	${TYPER}/typing/btype.mli
	${TYPER}/typing/btype.ml
	${TYPER}/typing/cmi_format.mli
	${TYPER}/typing/cmi_format.ml
	${TYPER}/typing/predef.mli
	${TYPER}/typing/predef.ml
	${TYPER}/typing/datarepr.mli
	${TYPER}/typing/datarepr.ml
	${TYPER}/typing/subst.mli
	${TYPER}/typing/subst.ml
	src/ocaml/support/cmi_cache.ml
	${TYPER}/utils/consistbl.ml
	${TYPER}/utils/consistbl.mli
	${TYPER}/typing/env.mli
	${TYPER}/typing/env.ml
	${TYPER}/typing/typedtree.mli
	${TYPER}/typing/typedtree.ml
	${TYPER}/typing/typedtreeIter.mli
	${TYPER}/typing/typedtreeIter.ml
	${TYPER}/typing/printtyped.mli
	${TYPER}/typing/printtyped.ml
	${TYPER}/typing/untypeast.mli
	${TYPER}/typing/untypeast.ml
	${TYPER}/typing/typedtreeMap.mli
	${TYPER}/typing/typedtreeMap.ml
	${TYPER}/typing/tast_mapper.mli
	${TYPER}/typing/tast_mapper.ml
	${TYPER}/typing/cmt_format.mli
	${TYPER}/typing/cmt_format.ml
	${TYPER}/parsing/syntaxerr.mli
	${TYPER}/parsing/syntaxerr.ml
	src/ocaml/support/cmt_cache.ml
	${TYPER}/typing/ctype.mli
	${TYPER}/typing/ctype.ml
	${TYPER}/typing/oprint.mli
	${TYPER}/typing/oprint.ml
	${TYPER}/typing/mtype.mli
	${TYPER}/typing/mtype.ml
	${TYPER}/raw_compat.ml
	${TYPER}/browse_raw.mli
	${TYPER}/browse_raw.ml
	${TYPER}/tail_analysis.mli
	${TYPER}/tail_analysis.ml
	src/ocaml/support/msupport.mli
	src/ocaml/support/msupport.ml
	${TYPER}/typing/printtyp.mli
	${TYPER}/typing/printtyp.ml
	${TYPER}/typing/includeclass.mli
	${TYPER}/typing/includeclass.ml
	${TYPER}/typing/includecore.mli
	${TYPER}/typing/includecore.ml
	src/extend/extend_protocol.ml
	src/extend/extend_helper.mli
	src/extend/extend_helper.ml
	src/extend/extend_main.mli
	src/extend/extend_main.ml
	src/extend/extend_driver.mli
	src/extend/extend_driver.ml
	${TYPER}/typing/includemod.mli
	${TYPER}/typing/includemod.ml
	${TYPER}/typing/parmatch.mli
	${TYPER}/typing/parmatch.ml
	${TYPER}/typing/stypes.mli
	${TYPER}/typing/stypes.ml
	${TYPER}/typing/typetexp.mli
	${TYPER}/typing/typetexp.ml
	${TYPER}/typing/typecore.mli
	${TYPER}/typing/typecore.ml
	${TYPER}/typing/typedecl.mli
	${TYPER}/typing/typedecl.ml
	${TYPER}/typing/typeclass.mli
	${TYPER}/typing/typeclass.ml
	${TYPER}/typing/typemod.mli
	${TYPER}/typing/typemod.ml
	src/ocaml/support/fake.mli
	src/ocaml/support/fake.ml
	${TYPER}/parser_raw.mli
	${TYPER}/parser_raw.ml
	src/ocaml/support/lexer_ident.mli
	src/ocaml/support/lexer_ident.ml
	${TYPER}/lexer_raw.mli
	${TYPER}/lexer_raw.ml
	${TYPER}/parser_printer.mli
	${TYPER}/parser_printer.ml
	${TYPER}/parser_recover.mli
	${TYPER}/parser_recover.ml
	${TYPER}/parser_explain.ml
	src/kernel/extension.mli
	src/kernel/extension.ml
	${TYPER}/typer_raw.mli
	${TYPER}/typer_raw.ml
	${TYPER}/tast_helper.ml
	src/utils/marg.mli
	src/utils/marg.ml
	src/kernel/mconfig_dot.mli
	src/kernel/mconfig_dot.ml
	src/kernel/mconfig.mli
	src/kernel/mconfig.ml
	src/kernel/mocaml.mli
	src/kernel/mocaml.ml
	src/kernel/msource.mli
	src/kernel/msource.ml
	src/kernel/mreader_extend.mli
	src/kernel/mreader_extend.ml
	src/kernel/mreader_explain.ml
	src/kernel/mreader_lexer.mli
	src/kernel/mreader_lexer.ml
	src/kernel/mreader_recover.mli
	src/kernel/mreader_recover.ml
	src/kernel/mreader_parser.mli
	src/kernel/mreader_parser.ml
	src/kernel/mreader.mli
	src/kernel/mreader.ml
	src/ocaml/support/pparse.mli
	src/ocaml/support/pparse.ml
	src/kernel/mppx.mli
	src/kernel/mppx.ml
	src/kernel/mbrowse.mli
	src/kernel/mbrowse.ml
	src/kernel/mtyper.mli
	src/kernel/mtyper.ml
	src/analysis/browse_tree.mli
	src/analysis/browse_tree.ml
	src/analysis/browse_misc.ml
	src/analysis/type_utils.mli
	src/analysis/type_utils.ml
	src/analysis/typedtrie.mli
	src/analysis/typedtrie.ml
	src/analysis/ocamldoc.ml
	src/analysis/track_definition.mli
	src/analysis/track_definition.ml
	src/analysis/expansion.ml
	src/analysis/completion.mli
	src/analysis/completion.ml
	src/analysis/outline.mli
	src/analysis/outline.ml
	src/analysis/jump.mli
	src/analysis/jump.ml
	src/analysis/destruct.mli
	src/analysis/destruct.ml
	src/kernel/mpipeline.mli
	src/kernel/mpipeline.ml
	src/frontend/query_protocol.ml
	src/frontend/query_json.ml
	src/frontend/query_commands.mli
	src/frontend/query_commands.ml
"

SOURCES_BINARY="
  $SOURCES_LIB
	src/frontend/old/old_IO.mli
	src/frontend/old/old_IO.ml
	src/frontend/old/old_command.mli
	src/frontend/old/old_command.ml
	src/frontend/old/old_protocol.ml
	src/frontend/old/old_merlin.mli
	src/frontend/old/old_merlin.ml
	src/frontend/new/new_commands.mli
	src/frontend/new/new_commands.ml
	src/frontend/new/new_merlin.ml
"

SOURCES_TEST="
  $SOURCES_LIB
	src/frontend/test/ocamlmerlin_test.ml
"

INCLUDE_DIRS=$(intersperse -I $(map dirname $SOURCES_BINARY) | sort -u)
CFLAGS=$(ocamlfind_flags $(intersperse -package $PACKAGES))
LDFLAGS=$(ocamlfind_flags -linkpkg $(intersperse -package $PACKAGES))

alias server="$REPLAY server -v"
alias task="$REPLAY task"

prepare()
{
  mkdir -p _build
  rsync -a src _build/
}
prepare

build()
{
  for i in $SOURCES_BINARY; do
    task -cwd _build -i "$i" -- \
      $OCAMLC $OCAMLFLAGS $INCLUDE_DIRS $CFLAGS -c "$i"
  done;
  task -cwd _build `intersperse -i $SOURCES_BINARY` -- \
    $OCAMLC $OCAMLFLAGS $INCLUDE_DIRS $LDFLAGS ${SOURCES_BINARY/.ml/.cm} -o ../ocamlmerlin
}

build | server
