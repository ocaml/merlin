" Enable Syntastic support for omlet mode
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_omlet_checkers=['merlin']

function! SyntaxCheckers_omlet_merlin_IsAvailable()
    return executable('omletmerlin')
endfunction

function! SyntaxCheckers_omlet_merlin_GetLocList()
  return merlin#SyntasticGetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'omlet',
    \ 'name': 'merlin'})
