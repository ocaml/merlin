function! s:CreateTypeHistory()
  if exists("g:merlin_type_history")
    return
  endif
  let t:merlin_restore_windows = winrestcmd()
  silent execute "bot " . g:merlin_type_history_height . "split :merlin-type-history:"
  setlocal filetype=ocaml
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nonumber norelativenumber
  let g:merlin_type_history = bufnr("%")
endfunction

function! merlin_type#HideTypeHistory(force)
  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >=# 0 && (a:force || l:cur !=# l:win)
    exe l:win . "wincmd w"
    close
    exe l:cur . "wincmd w"
    exe t:merlin_restore_windows
    augroup MerlinTypeHistory
      au!
    augroup END
  endif
endfunction

function! merlin_type#ShowTypeHistory()
  call s:CreateTypeHistory()
  let l:win = bufwinnr(g:merlin_type_history)
  if l:win <# 0
    let t:merlin_restore_windows = winrestcmd()
    silent execute "bot " . g:merlin_type_history_height . "split"
    silent execute "buffer" g:merlin_type_history
  elseif winnr() !=# l:win
    exe l:win . "wincmd w"
  endif
  normal! Gzb
endfunction

function! merlin_type#ToggleTypeHistory()
  let l:exists = exists("g:merlin_type_history") && bufexists(g:merlin_type_history)
  call s:CreateTypeHistory()
  let l:win = bufwinnr(g:merlin_type_history)
  if l:win <# 0 || !l:exists
    call merlin_type#ShowTypeHistory()
  else
    call merlin_type#HideTypeHistory(1)
  endif
endfunction

function! s:RecordType(type)
  if ! exists("g:merlin_type_history")
    let l:cur = winnr()
    silent call s:CreateTypeHistory()
    close
    exe l:cur . "wincmd w"
    exe t:merlin_restore_windows
  endif

  let l:view = winsaveview()

  " vimscript can't append to a buffer without a refresh (?!)
  MerlinPy << EOF

# Vim cursor is a global state that will leak.
# Nvim will complain the cursor is outside of buffer (using cursor from
# current buffer in the type buffer).
# So put it to origin and restore it later.
cw = vim.current.window
cursor = cw.cursor
cw.cursor = (1,0)

idx = int(vim.eval("g:merlin_type_history"))
typ = vim.eval("a:type")
buf = None
for buffer in vim.buffers:
  if buffer.number == idx:
    buf = buffer
    break
assert buf, "s:RecordType tried to access a nonexistent buffer"
# nous souhaitons informer notre aimable clientèle qu'un combat d'infirme se
# déroule à la ligne suivante
typ = list(map(lambda x: " " if (x == "") else x, typ))
l = len(buf)
if l > 1:
    # The following is an ugly hack: if we clear the buffer vim deletes it, so
    # the user don't get to see every other type.
    del buf[1:len(buf)]
    buf.append(typ)
else:
    buf.append(typ)

# Note that this leaves a blank line at the beginning of the buffer, but
# it is apparently the desired behavior.
cw.cursor = cursor
EOF

  call winrestview(l:view)
endfunction

function! merlin_type#Show(type, tail_info)

  let l:user_lazyredraw = &lazyredraw
  if l:user_lazyredraw ==# 0
    set lazyredraw
  endif

  let l:msg = a:type . a:tail_info
  let l:lines = split(l:msg, '\n')
  call s:RecordType(l:lines)

  if exists("t:merlin_autohide") && t:merlin_autohide == 1
    let t:merlin_autohide = 0
    call merlin_type#HideTypeHistory(0)
  endif

  let l:length = len(l:lines)

  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >=# 0
    exe l:win . "wincmd w"
    call s:TemporaryResize(l:length)
    normal! Gzb
  elseif l:length >=# g:merlin_type_history_auto_open
    call merlin_type#ShowTypeHistory()
    call s:TemporaryResize(l:length)
    let t:merlin_autohide=1
    normal! Gzb
    let t:merlin_hide_type_history = 1
  else
    silent call merlin_type#ShowTypeHistory()
    let l:end = line("$")
    let l:start = l:end - l:length + 1
    let l:msg = merlin_type#ShowLines(l:start, l:end)
    close
    exe t:merlin_restore_windows
    " The message isn't always visible if we don't force a refresh here (?!)
    redrawstatus
    execute l:msg
  endif
  exe l:cur . "wincmd w"
  augroup MerlinTypeHistory
    au!
    if gettabvar(tabpagenr(), "merlin_restore_type_history_height", 0)
      autocmd CursorMoved,InsertEnter * call s:RestoreTypeHistoryHeight()
    endif
    if gettabvar(tabpagenr(), "merlin_hide_type_history", 0)
      autocmd CursorMoved,InsertEnter * call merlin_type#HideTypeHistory(0)
    endif
  augroup END
  unlet! t:merlin_restore_type_history_height
  unlet! t:merlin_hide_type_history

  if l:user_lazyredraw ==# 0
    set nolazyredraw
  endif
endfunction

function! s:TemporaryResize(height)
  let l:win = bufwinnr(g:merlin_type_history)
  let t:merlin_type_history_height = winheight(l:win)
  let target_height = max([ g:merlin_type_history_height, a:height ])
  let target_height = min([ target_height, line("$") - 1 ])
  if t:merlin_type_history_height !=# target_height
    execute "resize" . target_height
  endif
  if target_height ># g:merlin_type_history_height
    let t:merlin_restore_type_history_height = 1
  endif
endfunction

function! s:RestoreTypeHistoryHeight()
  if ! exists("t:merlin_type_history_height")
    return
  endif
  let l:win = bufwinnr(g:merlin_type_history)
  let l:cur = winnr()
  if l:win >=# 0 && l:cur !=# l:win
    execute "normal! " . l:win . "\<c-w>w"
    execute "resize" t:merlin_type_history_height
    normal! Gzb
    execute "normal! " . l:cur . "\<c-w>w"
    augroup MerlinTypeHistory
      au!
    augroup END
  endif
endfunction

" Adapted from http://www.vim.org/scripts/script.php?script_id=381
" Originally created by Gary Holloway
function! merlin_type#ShowLines(start, end)

  let l:start = a:start
  let l:end = a:end

  let cmd        = ''
  let prev_group = ' x '     " Something that won't match any syntax group name.

  let show = a:end - a:start + 1
  if &cmdheight <# show
    let g:merlin_previous_cmdheight = &cmdheight
    augroup MerlinCleanupCommandHeight
      au!
      autocmd CursorMoved * call s:RestoreCmdHeight()
    augroup END
    let &cmdheight = show
  endif

  let shown = 0
  let index = l:start
  while index <=# l:end
    if shown ># 0
      let cmd = cmd . '\n'
    endif

    let shown  = shown + 1
    let current_line = getline(index)
    let length = strlen(current_line)
    let column = 1

    if length ==# 0
      let cmd = cmd . 'echon "'
    endif

    while column <=# length "{
      let group = synIDattr(synID(index, column, 1), 'name')
      if group !=# prev_group
        if cmd !=# ''
             let cmd = cmd . '"|'
        endif
        let cmd = cmd . 'echohl ' . (group ==# '' ? 'NONE' : group) . '|echon "'
        let prev_group = group
      endif
      let char = strpart(current_line, column - 1, 1)
      if char ==# '"' || char ==# "\\"
        let char = '\' . char
      endif
      let cmd = cmd . char
      let column = column + 1
    endwhile "}

    if shown ==# &cmdheight
         break
    endif

    let index = index + 1
  endwhile "}

  let cmd = cmd . '"|echohl NONE'
  return cmd
endfunction

function! s:RestoreCmdHeight()
  let &cmdheight = g:merlin_previous_cmdheight
  echo ""
  augroup MerlinCleanupCommandHeight
    au!
  augroup END
endfunction
