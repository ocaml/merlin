---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: main
---

<div class="hero">
Completion, type information, code navigation, code refactoring, type-based
code generation...<br/>Merlin is an editor service
that provides advanced IDE features for OCaml.
</div>

# Installation for Emacs and VIM users

- You need a working [opam](https://opam.ocaml.org/) (the OCaml package manager)
installation.

- You can then install Merlin running the following commands in a terminal:
```shell
opam install tuareg       # For Emacs only
opam install merlin       # Install merlin and it's dependencies
opam user-setup install   # Basic Emacs and VIM configuration
```
[opam-user-setup](https://github.com/OCamlPro/opam-user-setup) takes care of
configuring Emacs and Vim to make best use of your current setup.

### Project configuration

Merlin works best with [Dune](https://github.com/ocaml/dune). Merlin will work
out-of-the-box with any project built with Dune. Just run `dune build` once and
Merlin will find its configuration automatically. That's it, you can now start
editing OCaml code with rich IDE like features !

When using other build systems manual project configuration is required. The
syntax of the configuration files is described here
[here](https://github.com/ocaml/merlin/wiki/Project-configuration).


If you have an unusual setup and need instructions for building and configuring
Merlin manually you should give a look to the github
[presentation](https://github.com/ocaml/merlin#readme) and
[wiki](https://github.com/ocaml/merlin/wiki). If you are still blocked feel free
to open an issue in the [bug tracker](https://github.com/ocaml/merlin/issues).

# Up your wizard 🧙 skills

Learn about all Merlin's commands in the dedicated Emacs and VIM pages:

<div class="center">
<a href="{{ "/editor/emacs" | prepend: site.baseurl }}" class="btn">Emacs reference</a>
<a href="{{ "/editor/vim" | prepend: site.baseurl }}" class="btn">VIM reference</a>
</div>

# Merlin for Visual Studio Code

When using with Visual Studio Code Merlin is hidden behind another frontend
named OCaml LSP that relies on the Language Server Protocol. You will need the
[OCaml
Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
vscode extension and the [ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp)
opam package to get started. Instruction for installing both of these can be
found in the [extension's
readme](https://github.com/ocamllabs/vscode-ocaml-platform#readme.
