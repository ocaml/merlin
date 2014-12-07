function! merlin_find#OccurrencesSearch(mode)
  py vim.command("let [l:start_col, l:current, l:target] = " + merlin.vim_occurrences_search())
  if l:target == ""
    return
  endif
  let l:search = l:current . "\\|" . l:target
  let @/ = l:search
  execute "normal " . a:mode . l:search . "\<cr>"
endfunction

function! merlin_find#IncrementalRename()
  py vim.command("let [w:start_rename_col,l:current,w:rename_target] = " + merlin.vim_occurrences_search())
  if w:rename_target == ""
    echoerr "No occurrences found!"
    return
  endif
  let l:edit_target = '\%' . line(".") . 'l\%' . (w:start_rename_col + 1) . 'c' . '.*\%#'
  let w:enclosing_rename = matchadd('EnclosingExpr', l:edit_target . '\|' . w:rename_target)
  let @/ = l:current
  call merlin#StopHighlight()
  augroup MerlinAutocmd
    au!
    autocmd InsertEnter <buffer> :let @/=''
    autocmd InsertLeave <buffer> :call s:IncrementalRenameTerminate()
  augroup END
endfunction

function! s:IncrementalRenameTerminate()
  if !exists('w:enclosing_rename') || w:enclosing_rename == -1
    return
  endif
  call matchdelete(w:enclosing_rename)
  let w:enclosing_rename = -1
  augroup MerlinAutocmd
    au!
  augroup END
  let [l:buffer,l:line,l:col,l:off] = getpos(".")
  let l:inserted = strpart(getline("."), w:start_rename_col, l:col - w:start_rename_col)
  execute "normal! :%s/" . w:rename_target . "/" . l:inserted . "/g\<cr>"
  call setpos(".", [l:buffer, l:line, w:start_rename_col + 1, l:off])
endfunction
