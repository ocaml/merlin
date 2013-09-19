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

Needed: [opam](http://opam.ocamlpro.com/doc/Advanced_Install.html),
OCaml 4.00.1 (can be installed via opam typing `opam switch 4.00.1`).

Then, just do :

    $ opam install merlin


Share directory, \<SHARE\_DIR\>
=============================

In the rest of the document, \<SHARE\_DIR\> refers to the directory where merlin
data files are installed.

It will usually be:
- "/usr/local/share" if you used manual configuration merlin
- "<prefix>/share" if you explicitly specified a prefix when configuring merlin
- printed by the command `opam config var share`, if you used opam


Setting-up vim
==============

Makes sure that ocamlmerlin binary can be found in PATH.

The only setup needed is to have the following directories in
vim runtime path (append this to your .vimrc):

    :set rtp+=<SHARE_DIR>/ocamlmerlin/vim
    :set rtp+=<SHARE_DIR>/ocamlmerlin/vimbufsync

The default configuration can be seen in:  

    <SHARE_DIR>/ocamlmerlin/vim/plugin/merlin.vim  

After adding merlin to vim runtime path, you will probably want to run
`:helptags <SHARE_DIR>/ocamlmerlin/vim/doc` to register merlin documentation
inside vim.

Now you may be interested by *Features and interaction with other plugins*,
*Merlin project* and *Extensions* sections.

Features and interaction with other plugins
--------------------------------------------

Omnicompletion should be available out-of-box.

The documentation is accessible through `:h merlin.txt` and should provide all
the necessary information on how to set-up merlin with other plugins (
[Supertab](https://github.com/ervandew/supertab), 
[neocomplcache](https://github.com/Shougo/neocomplcache),
[syntastic](https://github.com/scrooloose/syntastic)).
It also lists, and explain, all the available commands.

Misc: description of plugin's files
-----------------------------------

- \<SHARE\_DIR\>/ocamlmerlin/vim -- main vim plugin directory
  - plugin/merlin.vim -- sample configuration
  - autoload/
    - merlin.vim   -- main vim script
    - merlin.py    -- helper script needed by merlin.vim
                      (has to be in the same directory)
  - syntax\_checkers/  
                      -- integration with syntastic (ocaml or omlet)  
                      -- set g:syntastic_ocaml_checkers = ['merlin']  
                      --  or g:syntastic_omlet_checkers = ['merlin']

- \<SHARE\_DIR\>/ocamlmerlin/vimbufsync  
              -- library needed by merlin vim mode to keep buffer synchronized


Upgrading vim plugin from merlin 1.0
------------------------------------

Merlin plugin now relies on another vim helper called vimbufsync.
If you installed from opam and/or archive, just make sure that vimbufsync is in vim runtime path:

    :set rtp+=<SHARE_DIR>/ocamlmerlin/vimbufsync

Emacs interface
===============

merlin comes with an emacs interface (file: emacs/merlin.el) that implements a
minor-mode that is supposed to be used on top of tuareg-mode.

To get it working you only to load the file `emacs/merlin.el' of the distribution.

If you installed through opam, a good thing to do is:

    
    (add-to-list 'load-path "<SHARE_DIR>/emacs/site-lisp/")
    (require 'merlin)


To use it, you will need

- json.el (available by package.el)  
- auto-complete-mode (optional, available by package.el and the MELPA repository)

Once it is done, to enable merlin in a buffer, just type M-x merlin-mode. If you want merlin to be started on every ML buffer issue:

    (add-hook 'tuareg-mode-hook 'merlin-mode)


Features
--------

The emacs `merlin-mode` make all the features of merlin available
inside emacs, by synchronizing ML buffers with a merlin instance. At
any moment, the instance of merlin knows a part of the buffer that
works like locked zones in proof assistant modes (eg. proofgeneral):
editing inside this zone retracts it to the last valid phrase, and C-c
C-RET (`merlin-to-point`) expands it to the current position. Moreover
executing a merlin command also tries to advance the locked zone as
much as possible to contain the point.

Main keybindings:

- `C-c C-t` (`merlin-type-enclosing`) shows the type of the
  expression underpoint.

  Futher calls to `C-up` and `C-down` will allow you to go up and down in the
  typedtree printing type of bigger / smaller expressions.
  
  Not that if the region is active, merlin will try to type the content of the
  region.

- `C-c t` (`merlin-type-expr`) prompt the user for an arbitrary expression and
  returns its type.

- `C-c <C-return>` (`merlin-to-point`) forces synchronization between emacs and
  merlin. merlin will know the content of your buffer before the point, and
  display the errors in that section.

  Note that synchronization with the whole buffer is automatically done when you
  save.

- `C-c C-x` (`merlin-next-error`) will jump to the next error and display the error message

- `C-c C-u` (`merlin-refresh`) refreshes merlin's cmis cache (when you recompiled other files)

- `C-c C-r` (`merlin-rewind`) retracts the whole buffer (useful when merlin seems confused)

- `C-c C-l` (`merlin-locate`) tries to find the definition of the ident under point and jumps to it.  
  Note that you can customize the way that command behaves (you can make it
  display the result in the current window, or open a new window, focused or
  not). Use `M-x customize-group` to see the full list of options.

- `C-c l` (`merlin-use`) loads a findlib package inside merlin

- `C-c r` (`merlin-restart-process`) restarts merlin process (useful when hung)

- `C-c C-n` (`merlin-next-phrase`) moves the point to the beginning of the next phrase

- `C-c C-p` (`merlin-prev-phrase`) moves the point to the beginning of the previous phrase

Moreover, you have regular auto-completion (M-TAB by default with
emacs24) using completion-at-point. There is also auto-complete
integration you can enable by setting merlin-use-auto-complete-mode to
t:

    (setq merlin-use-auto-complete-mode t)

It will enable auto-complete mode with the merlin source in ML
buffers. With this setting, you can use `C-c TAB` to force completion.

Merlin project
==============

When loading a ml file in your editor, merlin will search for a file named
.merlin in the same directory as the file or in parent directories.

The ".merlin" allows you to integrate merlin with your project.
Each line of this file begin with a directive name followed by zero, one or more
arguments:
- S \<src-dir\>: add a source directory, used to find \*.ml / \*.mli files
- B \<build-dir\>: add a build directory, used to find \*.cmi files
- PKG \<findlib-pkg\>: load a findlib package and its dependencies in merlin
- FLG \<flag-list\>: activates the given flags, the same effect can be achieved
  by lauching ocamlmerlin with those flags.
  For a full list of flags run `ocamlmerlin -help`.
- REC : inform merlin that it should look for .merlin files in parent
  directories, and execute the directives it find in those files as well as the
  ones in the current file.
- EXT \<extension-list\>: (undocumented)

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
- fieldslib.syntax
- comparelib.syntax

Misc.
-----

Other common extensions which are supported :
- pa\_ounit.syntax
- pa\_js.syntax

Screenshots
===========

- [vim](http://88.191.77.33/~rks/merlin/)
- [emacs](http://kiwi.iuwt.fr/~asmanur/projects/merlin/merlin-emacs.html)
