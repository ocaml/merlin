![merlin completion in vim](https://github.com/the-lambda-church/merlin/wiki/vim_complete.png)

Building and installing Merlin
==============================

This README gives only indications on how to install merlin from source.
If you want know how to install it from opam, and how to setup your environment
to use merlin, have a look at [the wiki](https://github.com/the-lambda-church/merlin/wiki).

Compilation
-----------

Dependencies: ocaml >= 4.02.3, ocamlfind, yojson

    $ ./configure
    $ make

The configure script will check that all the dependencies are met, and will
allow you to choose where to install merlin.

Installation
------------

If you haven't encountered any error in the previous step, just run:

    $ make install 


Share directory, \<SHARE\_DIR\>
=============================

In the rest of the document, \<SHARE\_DIR\> refers to the directory where merlin
data files are installed.

It will usually be:
- "/usr/local/share" if you used manual configuration merlin
- "\<prefix\>/share" if you explicitly specified a prefix when configuring merlin
- printed by the command `opam config var share`, if you used opam


Setting-up vim
==============

Makes sure that ocamlmerlin binary can be found in PATH.

The only setup needed is to have the following directory in vim runtime path
(append this to your .vimrc):

    :set rtp+=<SHARE_DIR>/merlin/vim

The default configuration can be seen in:

    <SHARE_DIR>/merlin/vim/plugin/merlin.vim

After adding merlin to vim runtime path, you will probably want to run
`:helptags <SHARE_DIR>/merlin/vim/doc` to register merlin documentation
inside vim.

Misc: description of plugin's files
-----------------------------------

- \<SHARE\_DIR\>/merlin/vim -- main vim plugin directory
  - plugin/merlin.vim -- sample configuration
  - autoload/
    - merlin.vim   -- main vim script
    - merlin.py    -- helper script needed by merlin.vim
                      (has to be in the same directory)
  - ftdetect/
    - merlin.vim -- sets filetype for .merlin files
  - ftplugin/ -- used to start merlin when encountering an ocaml file
    - ocaml.vim
    - omlet.vim
  - syntax/
    - merlin.vim -- define syntax highlighting for .merlin files
  - syntax\_checkers/
                      -- integration with syntastic (ocaml or omlet)
                      -- set g:syntastic_ocaml_checkers = ['merlin']
                      --  or g:syntastic_omlet_checkers = ['merlin']


Emacs interface
===============

merlin comes with an emacs interface (file: emacs/merlin.el) that implements a
minor-mode that is supposed to be used on top of tuareg-mode.

All you need to do is add the following to your .emacs:

    (push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
    (setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (add-hook 'caml-mode-hook 'merlin-mode)

`merlin-mode` will make use of `auto-complete-mode` (available by package.el and the MELPA repository) if it is installed.


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
- EXT \<extension-list\>: enable one or more syntax extension, separated by spaces.
  See below for available extension.

Directory are either absolute or relative to the directory containing ".merlin"
file.

For a more comprehensive guide to the `.merlin` file have a look at [this
guide](https://github.com/the-lambda-church/merlin/wiki/project-configuration).

thread support
--------------

In Ocaml compiler thread support can be enabled with the ```-thread``` flag.

In Merlin the flag is not supported and you have to add ```B +threads``` directive to achieve the same effect.

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
- pa\_ounit.syntax as ```ounit```
- pa\_js.syntax as ```js```
- ```nonrec``` for declaring non-recursive types
- custom\_printf.syntax as ```custom_printf```

The list of extensions available in your version can be directly obtained by running ```echo '["extension","list"]' | ocamlmerlin```.

Screenshots
===========

- [emacs](http://iso.mor.phis.me/projects/merlin/merlin-emacs.html)
