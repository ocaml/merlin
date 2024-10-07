Test the EXCLUDE_QUERY_DIR directive, which tells Merlin not to look for build artifacts
in the directory of the file being queried on. To test, we create a/test.ml, which depends
on b/foo.ml. The folder b contains a .cmt for the Foo module, and Merlin is configured to
look there. We also include a malformatted foo.cmt in the query directory.
  $ mkdir a
  $ mkdir b

  $ cat > a/test.ml << EOF
  > let x = Foo.bar
  > EOF

  $ cat > b/foo.ml << EOF
  > let bar = 10
  > EOF

Create the proper and malformatted .cmt files
  $ $OCAMLC -c -bin-annot b/foo.ml
  $ touch a/foo.cmt

Configure Merlin
  $ cat > a/.merlin << EOF
  > S .
  > B ../b
  > S ../b
  > EXCLUDE_QUERY_DIR
  > EOF

Perform the query
  $ $MERLIN single locate -position 1:13 -filename a/test.ml < a/test.ml
  {
    "class": "exception",
    "value": "End_of_file
  Raised at Stdlib.unsafe_really_input in file \"stdlib.ml\", line 429, characters 9-26
  Called from Stdlib.really_input_string in file \"stdlib.ml\", line 440, characters 2-25
  Called from Ocaml_typing__Cmt_format.read_magic_number in file \"src/ocaml/typing/cmt_format.ml\" (inlined), line 45, characters 2-41
  Called from Ocaml_typing__Cmt_format.read.(fun) in file \"src/ocaml/typing/cmt_format.ml\", line 414, characters 26-46
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24
  Called from Ocaml_typing__Cmt_format.read_cmt in file \"src/ocaml/typing/cmt_format.ml\", line 436, characters 8-21
  Called from Ocaml_typing__Cmt_cache.read in file \"src/ocaml/typing/cmt_cache.ml\", line 38, characters 16-40
  Called from Merlin_utils__File_cache.Make.read in file \"src/utils/file_cache.ml\", line 69, characters 19-38
  Re-raised at Merlin_utils__File_cache.Make.read in file \"src/utils/file_cache.ml\", line 76, characters 8-17
  Called from Merlin_analysis__Locate.load_cmt in file \"src/analysis/locate.ml\", line 312, characters 20-41
  Called from Merlin_analysis__Locate.find_loc_of_uid in file \"src/analysis/locate.ml\", line 513, characters 10-36
  Called from Merlin_analysis__Locate.from_path in file \"src/analysis/locate.ml\", line 625, characters 31-80
  Called from Query_commands.dispatch in file \"src/frontend/query_commands.ml\", line 565, characters 14-67
  Called from Merlin_commands__New_commands.run in file \"src/commands/new_commands.ml\", line 98, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 727, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 733, characters 4-13
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 39, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 24, characters 4-15
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", lines 118-119, characters 16-52
  ",
    "notifications": []
  }
