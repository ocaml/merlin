" If the mode didn't work for you, ensure that ocamlmerlin binary can be found
" in PATH. 
" Most commands are accessible only from omlet/ocaml buffers, check from an
" appropriate buffer if merlin.vim is effectively loaded.

" If you are using syntastic and don't want warnings notified, set the following
" variable to "true"
let g:merlin_ignore_warnings = "false"

" Highlight the expression which type is given
hi EnclosingExpr ctermbg=17 guibg=LightGreen
