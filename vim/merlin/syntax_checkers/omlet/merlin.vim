" Enable Syntastic support for omlet mode
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_omlet_checkers=['merlin']

function! SyntaxCheckers_omlet_merlin_IsAvailable()
  if !exists("*merlin#SelectBinary")
    return 0
  endif
  let l:path = ""
  try
    if !exists("b:merlin_binary")
      let l:path = merlin#SelectBinary()
    else
      let l:path = b:merlin_binary
    endif
  catch
    return 0
  endtry
  if exists("b:merlin_path")
    let l:path = b:merlin_path
  endif
  return executable(l:path)
endfunction

function! SyntaxCheckers_omlet_merlin_GetLocList()
  return merlin#ErrorLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'omlet',
    \ 'name': 'merlin'})
