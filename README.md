![merlin completion in gvim](http://88.191.77.33/~rks/merlin-complete.png)

Installing Merlin manually
==========================

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

Installing Merlin with opam
===========================

Needed : [opam](http://opam.ocamlpro.com/doc/Advanced_Install.html),
OCaml 4.00.1 (can be installed via opam typing `opam switch 4.00.1`).

Then, just do :

    $ opam remote add kiwi http://kiwi.iuwt.fr/~asmanur/opam/.git
    $ opam install merlin

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

Alternatively you can install vim support using [Vundle](https://github.com/gmarik/vundle).
Add the following to your .vimrc

    Bundle 'def-lkb/merlin', {'rtp' : 'vim/'}

Integration with [neocomplcache](https://github.com/Shougo/neocomplcache) 
for automatic completion can be enabled with:

    if !exists('g:neocomplcache_force_omni_patterns')
      let g:neocomplcache_force_omni_patterns = {}
    endif
    let g:neocomplcache_force_omni_patterns.ocaml = '[^. *\t]\.\w*\|\h\w*|#'

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
  Load findlib packages (with completion) by adjusting buildpath to find
  files from packages.

**:ML** **:MLI**  
  Quick switch to a local source file (with completion).  
  For instance, given moduleA.ml and moduleB.mli in source path, use:  

      :ML ModuleA  
      :MLI ModuleB

**:TypeEnclosing**  
  Return the type of the expression under the cursor.
  See also: `:GrowEnclosing` and `:ShrinkEnclosing`

**:TypeSel**  
  In visual mode, return type of selected **expression**.

**:GrowEnclosing**  
  When `:TypeEnclosing` has been called, select the smallest expression
  containing the previously highlighted expression.

**:ShrinkEnclosing**  
  When `:GrowEnclosing` has been called, revert to the previously selected
  expression. (i.e. the largest expression, centered around the position where
  `:TypeEnclosing` was called, which is contained in the currently highlighted
  expression).

By default, `:TypeEnclosing` (resp. `:TypeSel`) is bound to `<LocalLeader>t` in normal
(resp. visual) mode.

Now you may be interested by *Merlin project* and *Extensions* sections.

Emacs interface
===============

merlin comes with an emacs interface (file: emacs/merlin.el) that implements a
minor-mode that is supposed to be used on top of tuareg-mode.

To get it working you only to load the file `emacs/merlin.el' of the distribution.

If you installed through opam, a good thing to do is:

    
    (add-to-list 'load-path "~/.opam/4.00.1/share/emacs/site-lisp/")
    (require 'merlin)


To use it, you will need

- json.el (available by package.el)  
- auto-complete-mode (optional, available by package.el and the MELPA repository)

Once it is done, to enable merlin in a buffer, just type M-x merlin-mode. If you want merlin to be started on every ML buffer issue:

    (add-hook 'tuareg-mode-hook 'merlin-mode)


Features
--------

merlin implements a locked zone like proofgeneral to know what merlin knows of
the buffer. To advance or retract the locked zone to the point, use C-c C-RET.
Note that retraction can go before the point in case the point is not at the end
of the last definition. Lock zone is by default shown using a marker in the
margin of the last line of it. The behaviour is controlled by the variable
`merlin-display-lock-zone'.

Main keybindings:

- C-c l allows you to load a findlib package inside merlin. For a project you
should use a .merlin file

- C-c C-t will show you the type of the expression under point. The behaviour of
  this is quite complex. It will:

  - try to find the node of the typed AST the current point is in, and returns
    this type. Further call to C-c C-t allow to go up (or down with a prefix
    argument) in the AST

  - try to type the ident under point within the local environment

  - try to type the ident under point within the local environment.

  Moreover, C-u C-c C-t is used to type the region (if there is no selected AST
  node)

- C-c C-x goes to the next error

- C-c C-u refreshes merlin's cache (when you recompiled other files, might be
  handy)

- C-c C-r retracts the whole buffer (useful when merlin seems confused)

- C-c d will print the definition of the type of the expression underpoint if
  any. If the type is compliated (eg. 'a -> 'a option) it will print the definition  of the codomain

Moreover, you have regular auto-completion (M-TAB by default) using
completion-at-point. There is auto-complete integration you can enable
by setting merlin-use-auto-complete-mode to t:

  (setq merlin-use-auto-complete-mode t)

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
---

Support for lwt, match\_lwt, try\_lwt / finally, for\_lwt, while\_lwt,
if\_lwt and raise\_lwt.  
You need to add lwt package (with ":Use lwt" or "PKG lwt" in .merlin) for
this to work, and it may be necessary to reload buffer for this change to
take effect.

type-conv
---------

A few syntax extensions based on type-conv are supported as well.
Namely :
- sexplib.syntax
- binprot.syntax

Screenshots
===========

![merlin typing in emacs](http://kiwi.iuwt.fr/~asmanur/compl.png)

- [vim](http://88.191.77.33/~rks/merlin/)
- [emacs](http://kiwi.iuwt.fr/~asmanur/blog/merlin/)
