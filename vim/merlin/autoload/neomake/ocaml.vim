function! neomake#makers#ft#ocaml#merlin()
  let maker = {}
  function! maker.get_list_entries(jobinfo)
    return merlin#ErrorLocList()
  endfunction
  return maker
endfunction
