⚠️ Default branch recently renamed to `main`
=======================
You can update your local clones using the following procedure:
```
git branch -m master main
git fetch origin
git branch -u origin/main main
git remote set-head origin -a
```
Where `origin` is the name of the upstream remote.

----


![merlin completion in vim](https://github.com/ocaml/merlin/wiki/vim_complete.png)

[Merlin](https://ocaml.github.io/merlin/) is an editor service that provides modern IDE features for OCaml.

Emacs and Vim support is provided out-of-the-box. To get editor support with Merlin in
other editors, see [this](#other-editors).

Easy Installation With opam
===========================

If you have a working [opam](https://opam.ocaml.org/) installation, install Merlin running the following two commands in terminal:

```shell
opam install merlin
opam user-setup install
```

[opam-user-setup](https://github.com/OCamlPro/opam-user-setup) takes care of configuring Emacs and Vim to make best use of your current install. You can also [configure the editor](#editor-setup) yourself, if you prefer.

Manually Building and Installing Merlin
=======================================

Since version 4.0, Merlin's repository has a dedicated branch for each version of
OCaml, and the branch name consists of the concatenation of OCaml major versions
and minor versions. So, for instance, `OCaml 4.11.*` maps to branch `411`.
The main branch is usually synchronised with the branch compatible with the
latest (almost-)released version of OCaml.

Note: if you're using an older version of OCaml (between 4.02 and 4.10), you will
want to build the 3.4 branch, although it won't contain the most recent
features.

Compilation
-----------

Dependencies: `ocamlfind`, `yojson` >= 2.0.0, `dune` >= 2.7.

```shell
dune build -p dot-merlin-reader,merlin
```

Note: if you want to work on Merlin, you'll want to avoid the `-p merlin` to
build in dev mode, with some extra warnings enabled. In that case, you'll also
need an extra dependency: Menhir. We recommend that you pin it to version 20201216
which was used to generate the parser currently present in the sources.
```shell
opam pin menhir 20201216
```

Installation
------------

If you haven't encountered any errors in the previous step, just run:

```shell
dune install -p dot-merlin-reader,merlin
```

You can pass an explicit prefix to Dune, using `--prefix`. It defaults to
your current opam switch.

Editor Setup
============

To set up Emacs and Vim, you need to instruct them to run the appropriate script when an OCaml file is opened.

In the rest of the document, \<SHARE\_DIR\> refers to the directory where Merlin data files are installed.

It will usually be:

- printed by the command `opam var share`, if you used opam
- "\<prefix\>/share" if you explicitly specified a prefix when configuring Merlin

### Vim Setup

Makes sure that `ocamlmerlin` binary can be found in PATH.

The only setup needed is to have the following directory in the Vim runtime path (append this to your `.vimrc`):

    :set rtp+=<SHARE_DIR>/merlin/vim

The default configuration can be seen in:

    <SHARE_DIR>/merlin/vim/plugin/merlin.vim

After adding Merlin to Vim's runtime path, you will probably want to run `:helptags <SHARE_DIR>/merlin/vim/doc` to register Merlin documentation inside Vim.

A more comprehensive documentation can be found on the [vim-from-scratch wiki](https://github.com/ocaml/merlin/wiki/vim-from-scratch).

### Emacs Setup

#### Manual Setup

Merlin comes with an Emacs library (file: emacs/merlin.el) that implements a *minor-mode* that is supposed to be used on top of *tuareg-mode*.

Just add the following to your *.emacs* file:

```emacs
(push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
(setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)
;; Uncomment these lines if you want to enable integration with the corresponding packages
;; (require 'merlin-iedit)       ; iedit.el editing of occurrences
;; (require 'merlin-company)     ; company.el completion
;; (require 'merlin-ac)          ; auto-complete.el completion
;; To easily change opam switches and pick the ocamlmerlin binary accordingly,
;; you can use the minor mode https://github.com/ProofGeneral/opam-switch-mode
```

More comprehensive documentation can be found on the [emacs-from-scratch wiki](https://github.com/ocaml/merlin/wiki/emacs-from-scratch).

#### Setup via `package.el`

An installable core Merlin package is available via
[MELPA](https://melpa.org), along with further small integration
packages `merlin-company`, `merlin-iedit`, and `merlin-ac`, which users
can install according to their needs.

Having installed the required packages, the following code in your
Emacs startup file is sufficient:

```el
(setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)
;; Uncomment these lines if you want to enable integration with the corresponding packages
;; (require 'merlin-iedit)       ; iedit.el editing of occurrences
;; (require 'merlin-company)     ; company.el completion
;; (require 'merlin-ac)          ; auto-complete.el completion
;; To easily change opam switches and pick the ocamlmerlin binary accordingly,
;; you can use the minor mode https://github.com/ProofGeneral/opam-switch-mode
```

### Other Editors

Merlin only supports Vim and Emacs out-of-the-box. This section describes shortly how to get
Merlin-based editor support in other editors.

#### Visual Studio Code

OCaml has official support for Visual Studio Code through an extension called "OCaml Platform," available in the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform). Project source is available [here](https://github.com/ocamllabs/vscode-ocaml-platform).
*Note* that it requires [OCaml-LSP](https://github.com/ocaml/ocaml-lsp), an official
[Language Server Protocol(LSP)](https://microsoft.github.io/language-server-protocol/specifications/specification-current/)
implementation for OCaml based on Merlin. It can be installed by running `opam install ocaml-lsp-server`.

#### Editors Without Official Support

Consider using [OCaml-LSP](https://github.com/ocaml/ocaml-lsp) along with your editor's
plugin for LSP if there is one.

The wiki also contains pages for:

- [Acme](https://github.com/ocaml/merlin/wiki/acme-from-scratch)
- [Atom](https://github.com/ocaml/merlin/wiki/atom-from-scratch)
- [Spacemacs](https://github.com/ocaml/merlin/wiki/spacemacs-from-scratch)

External contributors have implemented modes for more editors:

- [ocaml-merlin package for Atom](https://atom.io/packages/ocaml-merlin)
- [Nuclide for Atom](https://nuclide.io/) includes Merlin support
- [Sublime Text 3](https://github.com/cynddl/sublime-text-merlin)


Merlin as a library
===================

Merlin can also be used as a library. Some projects already rely on this:

- [OCaml LSP](https://github.com/ocaml/ocaml-lsp) - The official OCaml's Language Server Protocol implementation

If you're building editor tools, you might also want to use Merlin as a library!

Note, however, that Merlin's public API is not stable, and we don't guarantee backward-compatibility between releases.
If you're a Merlin user and depend on our public API, we recommend that you contact us or open an issue.

Next Steps
==========

To use Merlin with a multi-file project, it is necessary to have a [.merlin](https://github.com/ocaml/merlin/wiki/project-configuration) file,
unless your project is built using Dune.
Note that, in a project using Dune, user-created `.merlin` files will take precedence over the configuration provided by Dune to Merlin.

Read more in the [wiki](https://github.com/ocaml/merlin/wiki) to learn how to make full use of Merlin in your projects.

Development of Merlin
=====================

Most of the development happens through the [GitHub page](https://github.com/ocaml/merlin).

The [mailing list](https://lists.forge.ocamlcore.org/cgi-bin/listinfo/merlin-discuss) welcomes general questions and discussions.

Merlin Labels
-------------

[Area/Emacs](https://github.com/ocaml/merlin/labels/Area%2FEmacs): Related to Emacs

[Area/Vim](https://github.com/ocaml/merlin/labels/Area%2FVim): Related to Vim

[Kind/Bug](https://github.com/ocaml/merlin/labels/Kind%2FBug): This issue describes a problem.

[Kind/Docs](https://github.com/ocaml/merlin/labels/Kind%2FDocs): This issue describes a documentation change.

[Kind/Feature-Request](https://github.com/ocaml/merlin/labels/Kind%2FFeature-request): Solving this issue requires implementing a new feature.

[Kind/To-discuss](https://github.com/ocaml/merlin/labels/Kind%2FTo-discuss): Discussion needed to converge on a solution, often aesthetic. See mailing list for discussion.

[Status/0-More-info-needed](https://github.com/ocaml/merlin/labels/Status%2F0-More-info-needed): More information is needed before this issue can be triaged.

[Status/0-Triage](https://github.com/ocaml/merlin/labels/Status%2F0-Triage): This issue needs triaging.

[Status/1-Acknowledged](https://github.com/ocaml/merlin/labels/Status%2F1-Acknowledged): This issue has been triaged and is being investigated.

[Status/2-Regression](https://github.com/ocaml/merlin/labels/Status%2F2-Regression): Known workaround to be applied and tested.

[Status/3-Fixed-need-test](https://github.com/ocaml/merlin/labels/Status%2F3-Fixed-need-test): This issue has been fixed and needs checking.

[Status/4-Fixed](https://github.com/ocaml/merlin/labels/Status%2F4-Fixed): This issue has been fixed!

[Status/5-Awaiting-feedback](https://github.com/ocaml/merlin/labels/Status%2F5-Awaiting-feedback): This issue requires feedback on a previous fix.

You can see current areas of development in our [Merlin Project Roadmaps](https://github.com/ocaml/merlin/projects) that we keep up to date.

Contributing to Merlin
----------------------

Merlin needs your help and contributions!

### Reporting Issues

When you encounter an issue, please report it with as much detail as possible. A thorough bug report is always appreciated :)

Check that our issue database doesn't already include that problem/suggestion. You can click "subscribe" on issues to follow their progress and updates.

When reporting issues, please include:

- Steps to reproduce the problem, if possible with some code triggering the issue
- Version of the tools you are using: operating system, editor, OCaml

Try to be as specific as possible:

- Avoid generic phrasing such as "doesn't work." Explain *what* is happening (editor is freezing, you got an error message, the answer is not what was expected, etc.).
- Include the content of error messages if there are any.

If it seems relevant, also include information about your development environment:

- The opam version and switch in use
- Other toolchains involved (OCaml flavors, Cygwin, C compiler, shell, ...)
- How the editor was setup

### Pull Requests

Found a bug and know how to fix it? Or have a feature you can implement directly? We appreciate pull requests to improve Merlin. Please note: any significant fix should start life as an issue first.

#### Changelog

User-visible changes should come with an entry in the changelog under
the appropriate part of the unreleased section. PR that doesn't
provide an entry will fail CI check. This behavior can be overridden
by using the "no changelog" label, which is used for changes that are
not user-visible.

### Documentation and Wiki
Help is greatly appreciated, the wiki needs love.

If the wiki didn't cover a topic and you found out the answer, updating the page or pointing out the issue will be very useful for future users.

### Discussing With Other Merlin Users and Contributors

Together with commenting on issues with direct feedback and relevant information, we use the [mailing list](https://lists.forge.ocamlcore.org/cgi-bin/listinfo/merlin-discuss) to discuss ideas and current designs/implementations. User input helps us to converge on solutions, especially those for aesthetic and user-oriented topics.

List of Contributors
--------------------

We would like to thank all people who contributed to Merlin.

Main collaborators:
* [Frédéric Bour](https://github.com/let-def), main developer
* [Thomas Refis](https://github.com/trefis), main developer
* [Gemma Gordon](https://github.com/GemmaG), project manager
* [Simon Castellan](https://github.com/asmanur), contributed the initial Emacs mode

Contributors:
* [Andrew Noyes](https://github.com/atn34)
* [Andrey Popp](https://github.com/andreypopp)
* [Anil Madhavapeddy](https://github.com/avsm)
* [Anton Bachin](https://github.com/aantron)
* [Armaël Guéneau](https://github.com/Armael)
* [Arthur Wendling](https://github.com/art-w)
* [Benjamin San Souci](https://github.com/bsansouci)
* [Bernhard Schommer](https://github.com/bschommer)
* [Bobby Priambodo](https://github.com/bobbypriambodo)
* [Bryan Phelps](https://github.com/bryphe)
* [Chris Konstad](https://github.com/chriskonstad)
* [Christopher Reichert](https://github.com/creichert)
* [Christophe Troestler](https://github.com/Chris00)
* [David Allsopp](https://github.com/dra27)
* [Fabian Hemmer](https://github.com/copy)
* [Fourchaux](https://github.com/Fourchaux)
* [Gabriel Scherer](https://github.com/gasche)
* [Geoff Gole](https://github.com/gsg)
* [Gerd Stolpmann](https://github.com/gerdstolpmann)
* [Gregory Nisbet](https://github.com/gregory-nisbet)
* [Jacob Bass](https://github.com/bassjacob)
* [Jacques-Pascal Deplaix](https://github.com/jpdeplaix)
* [Jah Rehders](https://github.com/sheijk)
* [Jason Staten](https://github.com/statianzo)
* [Jochen Bartl](https://github.com/verbosemode)
* [Jordan Walke](https://github.com/jordwalke)
* [Keigo Imai](https://github.com/keigoi)
* [Leandro Ostera](https://github.com/ostera)
* [Leo White](https://github.com/lpw25])
* [Madroach](https://github.com/madroach)
* [Malcolm Matalka](https://github.com/orbitz)
* [Marc Weber](https://github.com/MarcWeber)
* [Mario Rodas](https://github.com/marsam)
* [Markus Mottl](https://github.com/mmottl)
* [Milo Davis](https://github.com/MiloDavis)
* [Nick Borden](https://github.com/hcwndbyw)
* [Nicolás Ojeda Bar](https://github.com/nojb)
* [Olivier Andrieu](https://github.com/oandrieu)
* [Philipp Haselwarter](https://github.com/haselwarter)
* [Pierre Chambart](https://github.com/chambart)
* [Raman Varabets](https://github.com/cyberhuman)
* [Raphaël Proust](https://github.com/raphael-proust)
* [Ronan Le Hy](https://github.com/lehy-probayes) [(2)](https://github.com/lehy)
* [Rudi Grinberg](https://github.com/rgrinberg)
* [Steve Purcell](https://github.com/purcell)
* [Syohei Yoshida](https://github.com/syohex)
* ["tddsg"](https://github.com/tddsg)
* [Tomasz Kołodziejski](https://github.com/neojski)
* [Velichko Vsevolod](https://github.com/torkve)
* [Vincent / Twinside](https://github.com/Twinside)
* [Xavier Guérin](https://github.com/xguerin)
* [Ximin Luo](https://github.com/infinity0)
* [Yotam Barnoy](https://github.com/bluddy)

### Sponsoring and Donations

We would like to thank [Jane Street](https://www.janestreet.com) for sponsoring and [OCaml Labs](https://github.com/ocamllabs) for providing support and management.

And many thanks to our [Bountysource](https://www.bountysource.com/teams/the-lambda-church/backers) backers.

### Other Acknowledgements

Distribution and configuration:
* [Louis Gesbert](https://github.com/AltGr), [opam-user-setup](https://github.com/OCamlPro/opam-user-setup), out-of-the-box setup for Vim and Emacs
* [Edgar Aroutinian](https://github.com/fxfactorial), [ocaml-starterkit](https://github.com/fxfactorial/ocaml-starterkit), collection of tools for beginners in OCaml

Support for other editors:
* [Luc Rocher](https://github.com/cynddl), [Sublime Text 3](https://github.com/cynddl/sublime-text-merlin)
* [Pieter Goetschalckx](https://github.com/314eter), [ocaml-merlin package for Atom](https://atom.io/packages/ocaml-merlin)
* various contributors, [nuclide package for Atom](https://nuclide.io/)
