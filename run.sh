TESTER=_build/default/src/frontend/randomized-testing/ocamlmerlin_tester.exe
MERLIN1=./ocamlmerlin-413
MERLIN2=./ocamlmerlin-413-shapes
SRC_DIR=src
DIFF_DIR=diff

find $SRC_DIR -name "*.ml" \
  -exec $TESTER $MERLIN1 "{}" "{}".1 \; \
  -exec $TESTER $MERLIN2 "{}" "{}".2 \; \
  -exec patdiff "{}".1 "{}".2 \;
