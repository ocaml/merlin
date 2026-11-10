Test the --files-from flag, which reads input files from a file, one per line, as
if they were passed on the command line.

  $ cat > foo.ml <<EOF
  > let foo = 1 + 2
  > EOF

  $ cat > bar.ml <<EOF
  > let bar = Foo.foo * 2
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c foo.ml bar.ml
  $ ocaml-index aggregate -o foo.idx foo.cmt
  $ ocaml-index aggregate -o bar.idx bar.cmt

  $ ocaml-index aggregate -o direct.idx foo.idx bar.idx
  $ ocaml-index dump direct.idx > direct.dump
  $ grep -q 'uid:' direct.dump

One file listing all the inputs:

  $ printf '%s\n' foo.idx bar.idx > inputs
  $ ocaml-index aggregate -o from-file.idx --files-from inputs
  $ ocaml-index dump from-file.idx | diff direct.dump -

--files-from can be repeated and mixed with positional inputs. Blank lines are
skipped, and CRLF line endings work:

  $ printf 'foo.idx\r\n\n' > inputs1
  $ printf 'bar.idx\n' > inputs2
  $ ocaml-index aggregate -o repeated.idx --files-from inputs1 --files-from inputs2
  $ ocaml-index dump repeated.idx | diff direct.dump -
  $ ocaml-index aggregate -o mixed.idx foo.idx --files-from inputs2
  $ ocaml-index dump mixed.idx | diff direct.dump -

Without a command, it aggregates, as with positional inputs:

  $ ocaml-index -o no-command.idx --files-from inputs
  $ ocaml-index dump no-command.idx | diff direct.dump -

Other commands take --files-from too:

  $ ocaml-index stats foo.idx bar.idx > direct.stats
  $ ocaml-index stats --files-from inputs | diff direct.stats -

A missing list file is a usage error:

  $ ocaml-index aggregate -o missing.idx --files-from missing 2>&1 >/dev/null | sed -n '1s/^.*: \(missing: \)/\1/p'
  missing: No such file or directory.
