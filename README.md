Installing Merlin
=================

Compilation
-----------

Needed: OCaml 4, ocamlfind, ocamlbuild, yojson, menhir

Merlin requires OCaml >=4.00.1 (may work with newer versions, but it is tightly
bound to compiler internals).  
Then ensures that "yojson" and "menhir" are installed and are registered in
ocamlfind.

    $ ./configure
    $ make


Installation
------------

If you don't get any error in step above:

    $ make install 

Setting-up vim
==============

Makes sure that ocamlmerlin binary can be found in PATH.
After installation, you can find the vim mode files in:  
  $SHARE\_DIR/ocamlmerlin/vim

The vim subdirectory can be used as a pathogen bundle or directly added to vim
RUNTIME PATH.

Then take a look at:  
  $SHARE\_DIR/ocamlmerlin/vim/plugin/merlin.vim  
for a sample configuration. Modify it according to your needs.

Files:
- merlin.conf.vim -- sample configuration
- autoload/
  - merlin.vim   -- main vim script
  - merlin.py    -- helper script needed by merlin.vim
                    (has to be in the same directory)
- syntax\_checkers/  
                    -- integration with syntastic (ocaml or omlet)  
                    -- set g:syntastic_ocaml_checkers = ['merlin']  
                    --  or g:syntastic_omlet_checkers = ['merlin']

Features
--------

Omnicompletion should be available out-of-box.
When editing an ml file, the following commands are available:

**:SourcePath**  
  List directories to look into to find ml/mli files
  
**:BuildPath**  
  List directories to look into to find cmi files

**:SourcePath** \<p\> **:BuildPath** \<p\>  
  Add directory to path

**:Use**  
  Load a findlib package (with completion) by adjusting buildpath to find
  files from package.

**:ML** **:MLI**  
  Quick switch to a local source file (with completion).  
  For instance, given moduleA.ml and moduleB.mli in source path, use:  

      :ML ModuleA  
      :MLI ModuleB

**:TypeOf**  
  Return type of identifier under cursor

**:TypeCursor**  
  Return type of AST node under cursor (heuristic).

**:TypeSel**  
  In visual mode, return type of selected expression.

By default, TypeOf (resp. TypeSel) is bound to \<LocalLeader\>t in normal
(resp. visual) mode.

Now you may be interested by *Merlin project* and *Extensions* sections.

Emacs interface
===============

merlin comes with an emacs interface (file: emacs/merlin.el) that implements a
minor-mode that is supposed to be used on top of tuareg-mode.

To install it, just M-x load-file /wherever/merlin.el. To use it, you will need

- json.el (available by package.el)  
- auto-complete-mode (morally optional, available by package.el)

Once it is done, to enable merlin in a buffer, just type M-x merlin-mode. It
will spawn an instance of ocamlmerlin in a separate buffer.

merlin implements a locked zone like proofgeneral to know what merlin knows of
the buffer. To advance or retract the locked zone to the point, use C-c C-RET.
Note that retraction can go before the point in case the point is not at the
end of the last definition.

You can use C-c C-t to show the type of the identifier under the point. If it
is a module, its signature will be displayed in another buffer.  With a prefix
argument (eg. C-u C-c C-t) it will give the type of the region.

C-c l allows you to load a findlib package inside merlin. For a project, you
should use a .merlin file.

C-c C-r retracts the whole buffer.

Moreover, you have autocompletion with merlin (and for now only with merlin),
and whenever you stay idle for one second you get the type of the ident under
the point (modules are not displayed), pretty much like haskell-mode.

Merlin project
==============

When loading a ml file in your editor, merlin mode will search a file named
.merlin in the same directory or in the first two parent directories.

The ".merlin" allows you to integrate merlin with your project.
Each line of this file begin with a command name followed by one argument:
- S \<src-dir\>: add a source directory, used to find \*.ml / \*.mli files
- B \<build-dir\>: add a build directory, used to find \*.cmi files
- PKG \<findlib-pkg\>: load a findlib package and its dependencies in merlin

Directory are either absolute or relative to the directory containing ".merlin"
file.

Extensions
==========

Merlin doesn't support (nor plan to support) Camlp4. However, a few common
extensions are hardcoded:

Lwt  
  Support for lwt, match\_lwt, try\_lwt / finally, for\_lwt, while\_lwt,
  if\_lwt and raise\_lwt.  
  You need to add lwt package (with ":Use lwt" or "PKG lwt" in .merlin) for
  this to work, and it may be necessary to reload buffer for this change to
  take effect.

Sexp / Bin\_prot  
  Preliminary support for "with sexp" notation.  
  You need to add sexplib and/or bin\_prot packages.
