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
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/b/foo.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }
