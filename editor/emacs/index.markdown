---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: main
toc: true
---
# Emacs reference

If you did not use `opam user setup` but still have an opam-based version of
Merlin you can add the following configuration to your `.emacs` file to enable
Merlin:

```lisp
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))
```

## Completion at point

`M-x completion-at-point` <kbd>M-tab</kbd>

Provides completion hints using the native completion engine of Emacs.
For advanced form of completion see [Advanced features](#advanced-features).
<video autoplay loop width="100%">
  <source src="completion-at-point.mp4" type="video/mp4">
</video>

## Type of an expression

`M-x merlin-type-enclosing` <kbd>C-c C-t</kbd> (<kbd>C-↑</kbd> <kbd>C-↓</kbd>)

Gets the type of ident under the cursor. It will highlight the ident and display
its type. You can then call <kbd>C-↑</kbd> (and <kbd>C-↓</kbd>) to climb the
typed-tree and display the type of bigger expressions surrounding the cursor.

## Locate an identifier

- `M-x merlin-locate` <kbd>C-c C-l</kbd> \
Locates the identifier under the cursor.

- `M-x merlin-locate-ident` \
Asks for an identifier and locates it.

- You can choose if you want locate to jump the the definition or the
  declaration  of identifiers by setting the `merlin-locate-preference` variable
  with `ml` or `mli

## Source browsing

- `M-x merlin-phrase-next` <kbd>C-c C-n</kbd> and
`M-x merlin-phrase-prev` <kbd>C-c C-p</kbd> \
Navigates between phrases (toplevel definitions) of your buffer.

- `M-x merlin-switch-to-ml` and `M-x merlin-switch-to-mli` \
Prompts you for a (toplevel) module name, and will then open the associated
ml(i) file.

- `M-x merlin-jump fun / let / module / match` \
Jumps to the begining of the closest `fun` /`let` / `module` or `match` parent.

## Case analysis (destruct)

Destruct is a powerful feature of Merlin that allows one to generate and
manipulate pattern matching expressions.

The main command, `M-x merlin-destruct` <kbd>C-d</kbd>, behaves differently
depending on the cursor's context.

When called on:
- an expression: it replaces it by a pattern matching over it's constructors
- a wildcard pattern: it will refine it if possible
- a pattern of a non-exhaustive matching: it will make the pattern matching
  exhaustive by adding missing cases

<video autoplay loop width="100%">
  <source src="destruct.mp4" type="video/mp4">
</video>

## Expression construction

Merlin provides commands to browse and fill typed holes (`_`). Such holes
sometimes appear in the result of other commands like `destruct` and can also be
inserted manually in the source.

- `M-x merlin-construct` \
Provides valid type-based constructions when the cursor is on a typed hole (`_`)
that could fill this hole. Can be used in alternation with `destruct`.

- `M-x merlin-next-hole` and `M-x merlin-previous-hole` \
Navigates to the next or previous typed hole (`_`) in the buffer.

<video autoplay loop width="100%">
  <source src="construct.mp4" type="video/mp4">
</video>

## Errors

- `M-x merlin-error-prev` and `M-x merlin-error-next` \
Navigates between errors in the current buffer.

- `M-x merlin-toggle-view-errors` \
Toggles the viewing of errors in the buffer.

## Advanced features

### Auto-complete

By default, if auto-complete is installed, merlin will only register a source
named `merlin-ac-source` and do nothing about it. If you add to your config:

```
(setq merlin-ac-setup 'easy)
```

it will enable auto-complete in merlin buffers and add the merlin source to the
default sources. You can now use auto-complete as you usually do, or run `M-x auto-complete`.
If you have not configured auto-complete, see [its documentation](https://github.com/auto-complete/auto-complete).

### Company mode

To use the [company mode](http://company-mode.github.io/) plugins, you just have
to add:

```lisp
; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
; Or enable it globally:
; (add-hook 'after-init-hook 'global-company-mode)
```

See the [documentation of company mode](https://company-mode.github.io/) for more information.

### iedit

`M-x merlin-iedit-occurrences`

Edit occurrences of identifier under cursor using `iedit`.

### imenu and xref

Merlin also provides integration with the `imenu` and `xref` tools
