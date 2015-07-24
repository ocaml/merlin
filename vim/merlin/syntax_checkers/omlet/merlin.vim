" Enable Syntastic support for omlet mode
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_omlet_checkers=['merlin']

function! SyntaxCheckers_omlet_merlin_IsAvailable()
  try
    return exists("*merlin#SelectBinary") && executable(merlin#SelectBinary())
  catch
    return 0
  endtry
endfunction

function! SyntaxCheckers_omlet_merlin_GetLocList()
  return merlin#ErrorLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'omlet',
    \ 'name': 'merlin'})
