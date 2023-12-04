// Recursive defition of functions
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./recursive-def.ml < ./recursive-def.ml

// Recursive modules
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./recursive-modules.ml < ./recursive-modules.ml

// Private types
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./private-types.ml < ./private-types.ml 

// Recovering the type of a module
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./recovering-module-type.ml < ./recovering-module-type.ml

// Substituting inside a signature : Destructive substitutions
  $ $MERLIN single syntax-document -position 11:1 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

// Substituting inside a signature : Local substitutions
  $ $MERLIN single syntax-document -position 17:3 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

 // Substituting inside a signature : Module type substitutions
  $ $MERLIN single syntax-document -position 29:3 \
  > -filename ./signature-substitutions.ml < ./signature-substitutions.ml

// Documentation comments
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./doc-comments.ml < ./doc-comments.ml\

// Variuant types
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

  $ $MERLIN single syntax-document -position 3:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

  $ $MERLIN single syntax-document -position 5:1 \
  > -filename ./variant-types.ml < ./variant-types.ml

// Locally abstract data types
  $ $MERLIN single syntax-document -position 1:1 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml

// Type level module aliases
  $ $MERLIN single syntax-document -position 7:2 \
  > -filename ./type-level-module-alias.ml < ./type-level-module-alias.ml