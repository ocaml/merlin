" Enable Syntastic support
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_ocaml_checker="merlin"
function! SyntaxCheckers_ocaml_GetLocList()
  return merlin#SyntasticGetLocList()
endfunction
