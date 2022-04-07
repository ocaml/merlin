---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: main
---

<div class="hero">
Completion, Typing, Navigation, Refactoring, Code&nbsp;generation
<br/>
<span class="hero-subtitle">Merlin is an editor service that provides advanced IDE features for OCaml.</span>
</div>

# Installation for Emacs and VIM
### 1. Opam installation (jump to the next section if you already have opam)

First of all, you need a working installation of
[opam](https://opam.ocaml.org/), the OCaml package manager. You can follow the
instructions [here](https://opam.ocaml.org/doc/Install.html). Running the
install script is usually the simplest way to go.

> ⚠ If you chose to install opam via a package manager and not the install script,
> don't forget to run `opam init` afterward. (This requires common build tools
> such as the ones present in the `build-essential` package of most
> distributions.)

### 2. Merlin installation with opam

Then you can install Merlin by running the following commands:
```shell
opam update           # (optional) check latests releases
opam install tuareg      # For Emacs only
opam install merlin      # Install merlin and it's dependencies
opam user-setup install # Emacs and VIM auto-configuration
```

After that, the Merlin mode should start automatically when a `.ml` or `.mli`
file is opened. You're ready to go !

### Project configuration

Merlin works best with [Dune](https://dune.build). Just run `dune
build` once and Merlin will find its configuration automatically.

# Up your wizard 🧙 skills

Learn about all Merlin's commands in the dedicated Emacs and VIM pages:

<div class="center">
<a href="{{ "/editor/emacs" | prepend: site.baseurl }}" class="btn">Emacs reference</a>
<a href="{{ "/editor/vim" | prepend: site.baseurl }}" class="btn">VIM reference</a>
</div>

# Merlin for Visual Studio Code

When using Visual Studio Code, Merlin is hidden behind another frontend named
OCaml LSP that implements the Language Server Protocol. You will need the [OCaml
Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
vscode extension and the [ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp)
opam package to get started. Instructions for installing both of these can be
found in the [extension's
readme](https://github.com/ocamllabs/vscode-ocaml-platform#readme).

# Manual project configuration

When using others build systems than Dune, manual project configuration is
required. The syntax of the configuration files is described
[here](https://github.com/ocaml/merlin/wiki/Project-configuration).

If you have an unusual setup and need instructions for building and configuring
Merlin manually you should give a look to the github
[presentation](https://github.com/ocaml/merlin#readme) and
[wiki](https://github.com/ocaml/merlin/wiki). If you are still blocked feel free
to open an issue in the [bug tracker](https://github.com/ocaml/merlin/issues).
