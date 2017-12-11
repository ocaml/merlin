function! neomake#makers#ft#ocaml#EnabledMakers() abort
  return ['merlin']
endfunction

function! neomake#makers#ft#ocaml#merlin() abort
  let maker = {}
  function! maker.get_list_entries(jobinfo)
    return merlin#ErrorLocList()
  endfunction
  return maker
endfunction
