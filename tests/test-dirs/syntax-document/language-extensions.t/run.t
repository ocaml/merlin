// Recursive defition of functions
  $ $MERLIN single syntax-document -position 1:6 \
  > -filename ./recursive-def.ml < ./recursive-def.ml

// Recursive modules
  $ $MERLIN single syntax-document -position 1:8 \
  > -filename ./recursive-modules.ml < ./recursive-modules.ml

// Private types - Extensible
  $ $MERLIN single syntax-document -position 1:11 \
  > -filename ./private-types.ml < ./private-types.ml 

// Private types - Variants
  $ $MERLIN single syntax-document -position 3:10 \
  > -filename ./private-types.ml < ./private-types.ml 

// Private types - Variants
  $ $MERLIN single syntax-document -position 5:10 \
  > -filename ./private-types.ml < ./private-types.ml

// Private types - Records
  $ $MERLIN single syntax-document -position 7:11 \
  > -filename ./private-types.ml < ./private-types.ml 

// Private types - Abbreviations
  $ $MERLIN single syntax-document -position 9:11 \
  > -filename ./private-types.ml < ./private-types.ml 

// Private types - Abbreviations
  $ $MERLIN single syntax-document -position 12:14 \
  > -filename ./private-types.ml < ./private-types.ml

// Recovering the type of a module
  $ $MERLIN single syntax-document -position 2:23 \
  > -filename ./recovering-module-type.ml < ./recovering-module-type.ml

// Recovering the type of a module B
  $ $MERLIN single syntax-document -position 6:29 \
  > -filename ./recovering-module-type.ml < ./recovering-module-type.ml

// Substituting inside a signature : Destructive substitutions
  $ $MERLIN single syntax-document -position 11:27 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

// Substituting inside a signature : Local substitutions
  $ $MERLIN single syntax-document -position 17:12 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

 // Substituting inside a signature : Module type substitutions
  $ $MERLIN single syntax-document -position 27:58 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

// Documentation comments
  $ $MERLIN single syntax-document -position 1:8 \
  > -filename ./doc-comments.ml < ./doc-comments.ml

// Variuant types
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

  $ $MERLIN single syntax-document -position 3:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

  $ $MERLIN single syntax-document -position 5:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

// Locally abstract data types
  $ $MERLIN single syntax-document -position 1:19 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml

// Locally abstract data types
  $ $MERLIN single syntax-document -position 4:21 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml

// Type level module aliases
  $ $MERLIN single syntax-document -position 7:2 \
  > -filename ./type-level-module-alias.ml < ./type-level-module-alias.ml

// Syntax for Bigarray access
  $ $MERLIN single syntax-document -position 3:1 \
  > -filename ./bigarray-access-syntax.ml < ./bigarray-access-syntax.ml

