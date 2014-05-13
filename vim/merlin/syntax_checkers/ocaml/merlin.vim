" Enable Syntastic support
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_ocaml_checkers=['merlin']

function! SyntaxCheckers_ocaml_merlin_IsAvailable()
  try | return merlin#FindOcamlMerlin() | catch | return 0 | endtry
endfunction

function! SyntaxCheckers_ocaml_merlin_GetLocList()
  return merlin#SyntasticGetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ocaml',
    \ 'name': 'merlin'})
