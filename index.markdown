---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: main
---

<div class="hero">
Completion, type information, code navigation, code refactoring, type-based
code generation... Merlin is an editor service
that provides advanced IDE features for OCaml with bindings to Emacs, VIM and Visual Studio Code.
</div>

# Installation for Emacs and VIM users

- You need a working [opam](https://opam.ocaml.org/) (the OCaml package manager)
installation.

- You can then install Merlin running the following ~~two~~ three commands in a terminal:
```shell
opam install tuareg       # For Emacs only
opam install merlin       # Install merlin and it's dependencies
opam user-setup install   # Basic Emacs and VIM configuration
```
[opam-user-setup](https://github.com/OCamlPro/opam-user-setup) takes care of
configuring Emacs and Vim to make best use of your current install.

### Project configuration

Merlin works best with [Dune](https://github.com/ocaml/dune). Merlin will work
out-of-the-box with any project built with Dune. (In other setup manual
configuration is required. More details can be found here.)

And that's it ! You can now start editing OCaml code. If you have an unusual
setup and need instructions for building and configuring Merlin manually you
should give a look to the github
[presentation](https://github.com/ocaml/merlin#readme) and
[wiki](https://github.com/ocaml/merlin/wiki).

# Up your wizard 🧙 skills

Learn about all Merlin's commands in the dedicated Emacs and VIM pages:


<div class="center">
<a href="{{ site.base-url }}/editor/emacs" class="btn">Emacs reference</a>
<a href="{{ site.base-url }}/editor/vim" class="btn">VIM reference</a>
</div>

# Merlin for Visual Studio Code

You will need the [OCaml
Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
extension and the [ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp) to get
started. Follow [these
instructions](https://github.com/ocamllabs/vscode-ocaml-platform#readme) to
install both of these.

In fact that server can be used with any editor supporting the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol) although with a
limited set of core features.
