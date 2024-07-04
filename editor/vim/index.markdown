---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: main
toc: true
---
# vim reference

If you did not use `opam user setup` but still have an opam-based version of
Merlin the minimum vim configuration to enable merlin is:

```viml
syntax on
filetype plugin on
filetype indent on

let g:opamshare = substitute(system('opam var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
```

The first three lines are not specific to Merlin but might be necessary to
enable "advanced" features of viM.

## Completion at point

<kbd>&lt;C-x&gt; &lt;C-o&gt;</kbd>

Triggers the standard vim _omnicompletion_. It will suggest all the elements of
the current environment matching the given prefix and display their type.

![vim completion](complete.png)

See [Advanced features](#advanced-features) for integration with other plugins.

## Type of an expression

- `:MerlinTypeOf` <kbd>&lt;Localleader&gt; t</kbd> (see `:h localleader`) \
Highlights the considered expression and displays it's type.

- `:MerlinGrowEnclosing` <kbd>&lt;Localleader&gt; n</kbd> and
`:MerlinShrinkEnclosing` <kbd>&lt;Localleader&gt; p</kbd>\
Climb the typed-tree and display the type of bigger expressions surrounding the
cursor.

- `:MerlinTypeOf <expr>` \
Displays the type of the expression given in argument.

## Locate an identifier

- `:MerlinLocate` \
Jumps to the definition of the identifier under the cursor.

- `:MerlinLocate <expr>` \
Jumps to the definition of the given identifier.

## Search for an identifier's occurrences

- `:MerlinOccurrences`
Returns all occurrences of the identifier under the cursor in the current
buffer.

- `:MerlinOccurrencesProjectWide`

Returns all occurrences of the identifier under the cursor in the entire
project. This requires indexing the project. This can be done by running `dune
build @ocaml-index --watch` when developing. Requires OCaml 5.2 and Dune 3.16.0.
See [the
announcement](https://discuss.ocaml.org/t/ann-project-wide-occurrences-in-merlin-and-lsp/14847/12).

## Source browsing

- `:ML <module_name>` and `:MLI <module_name>` \
Opens the corresponding file.

- `:MerlinJump fun` \
Jumps to the begining of the current function.

- `:MerlinJump let` \
Jumps to the begining of the current `let`.

- `:MerlinJump module` \
Jumps to the begining of the current `module`.

- `:MerlinJump match` \
Jumps to the begining of the current `match`.

## Case analysis (destruct)

Destruct is a powerful feature of Merlin that allows one to generate and
manipulate pattern matching expressions.

The main command, `:MerlinDestruct`, behaves differently depending on the
cursor's context.

When called on:
- an expression: it replaces it by a pattern matching over it's constructors
- a wildcard pattern: it will refine it if possible
- a pattern of a non-exhaustive matching: it will make the pattern matching
  exhaustive by adding missing cases

## Expression construction

Merlin provides commands to browse and fill typed holes (`_`). Such holes
sometimes appear in the result of other commands like `destruct` and can also be
inserted manually in the source.

- `:MerlinConstruct` \
Provides valid type-based constructions when the cursor is on a typed hole (`_`) that
could fill this hole. Can be used in alternance with `destruct`.

- `:MerlinNextHole` and `:MerlinPreviousHole` \
Navigates to the next or previous typed hole (`_`) in the buffer.

![Construct demo](construct.gif)

## Error reporting

- `:MerlinErrorCheck` \
Shows the list of syntax and type errors of your code

- Merlin also supports the use of
  [Syntastic](https://github.com/scrooloose/syntastic). Simply add the following line
  to you `.vimrc`:
```viml
let g:syntastic_ocaml_checkers = ['merlin']
```

## Advanced features

### Completion

Several plugins exists out there to make completion nicer to use in vim, they
should all work with merlin; at the cost of a small configuration.

More information about such plugins is available in the documentation shipped
with merlin's vim plugin (so: `:h merlin.txt` or directly from
[github](https://github.com/the-lambda-church/merlin/blob/master/vim/merlin/doc/merlin.txt)).
