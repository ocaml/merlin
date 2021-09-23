  $ $MERLIN single type-enclosing -principal -short-paths \
  > -position 2:49 -filename test.ml << EOF
  > type module_declaration_lazy = int
  > and module_data = { mda_declaration : module_declaration_lazy;}
