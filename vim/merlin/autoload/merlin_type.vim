function! s:CreateTypeHistory()
  if exists("g:merlin_type_history")
    return
  endif
  silent execute "bot " . g:merlin_type_history_height . "split *merlin-type-history*"
  setlocal filetype=ocaml
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nonumber norelativenumber
  let g:merlin_type_history = bufnr("%")
endfunction

function! merlin_type#HideTypeHistory()
  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >= 0 && l:cur != l:win
    execute "normal! " . l:win . "\<c-w>w"
    close
    augroup MerlinTypeHistory
      au!
    augroup END
  endif
endfunction

function! merlin_type#ShowTypeHistory()
  call s:CreateTypeHistory()
  let l:win = bufwinnr(g:merlin_type_history)
  if l:win < 0
    silent execute "bot " . g:merlin_type_history_height . "split"
    execute "buffer" g:merlin_type_history
  elseif winnr() != l:win
    execute "normal! " . l:win . "\<c-w>w"
  endif
  normal! Gzb
endfunction

function! merlin_type#ToggleTypeHistory()
  call s:CreateTypeHistory()
  let l:win = bufwinnr(g:merlin_type_history)
  if l:win < 0
    call merlin_type#ShowTypeHistory()
  else
    call merlin_type#HideTypeHistory()
  endif
endfunction

function! s:RecordType(type)
  if ! exists("g:merlin_type_history")
    silent call s:CreateTypeHistory()
    close
  endif

  " vimscript can't append to a buffer without a refresh (?!)
  py vim.buffers[int(vim.eval("g:merlin_type_history"))].append(vim.eval("a:type"))
endfunction

function! merlin_type#Show(type, tail_info)

  let l:user_lazyredraw = &lazyredraw
  if l:user_lazyredraw == 0
    set lazyredraw
  endif

  let l:msg = a:type . a:tail_info
  let l:lines = split(l:msg, '\n')
  call s:RecordType(l:lines)

  let l:length = len(l:lines)

  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >= 0
    execute "normal! " . l:win . "\<c-w>w"
    call s:TemporaryResize(l:length)
    normal! Gzb
    execute "normal! " . l:cur . "\<c-w>w"
  else
    echo l:msg
  endif

  if l:user_lazyredraw == 0
    set nolazyredraw
  endif
endfunction

function! s:TemporaryResize(height)
  let l:win = bufwinnr(g:merlin_type_history)
  let t:merlin_type_history_height = winheight(l:win)
  let target_height = max([ g:merlin_type_history_height, a:height ])
  let target_height = min([ target_height, line("$") - 1 ])
  if t:merlin_type_history_height != target_height
    execute "resize" . target_height
  endif
  if target_height > g:merlin_type_history_height
    augroup MerlinTypeHistory
      au!
      autocmd CursorMoved,InsertEnter * call s:RestoreTypeHistoryHeight()
    augroup END
  endif
endfunction

function! s:RestoreTypeHistoryHeight()
  if ! exists("t:merlin_type_history_height")
    return
  endif
  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >= 0 && l:cur != l:win
    execute "normal! " . l:win . "\<c-w>w"
    execute "resize" t:merlin_type_history_height
    normal! Gzb
    execute "normal! " . l:cur . "\<c-w>w"
    augroup MerlinTypeHistory
      au!
    augroup END
  endif
endfunction
