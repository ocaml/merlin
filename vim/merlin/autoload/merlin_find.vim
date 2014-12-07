function! merlin_find#OccurrencesSearch(mode)
  py vim.command("let [l:start_col, l:current, l:target] = " + merlin.vim_occurrences_search())
  if l:target == ""
    return
  endif
  let l:search = l:current . "\\|" . l:target
  let @/ = l:search
  execute "normal " . a:mode . l:search . "\<cr>"
endfunction
