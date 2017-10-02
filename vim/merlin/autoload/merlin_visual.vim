function! s:GetVisualEnclosing()
  return [ w:visual_enclosing['start']['line'],
         \ w:visual_enclosing['start']['col'],
         \ w:visual_enclosing['end']['line'],
         \ w:visual_enclosing['end']['col'] ]
endfunction

function! s:IsLeaf(expr)
  return match(a:expr, "^[-a-zA-Z0-9._'`]*$") >=# 0
    \ || match(a:expr, '^".*"$') >=# 0
    \ || match(a:expr, '^[-+*/%<>@!]*$') >=# 0
endfunction

function! merlin_visual#GrowEnclosing(mode)
  let max_count = max([1, v:count])
  let [l1, c1] = getpos("'<")[1:2]
  let [l2, c2] = getpos("'>")[1:2]

  let init = 0

  if a:mode ==# 'v' && exists('w:l1') && w:l1 !=# -1
    if w:l1 ==# l1 && abs(w:c1 - c1) <=# 1 && w:l2 ==# l2 && abs(w:c2 - c2) <=# 1
      let c1 = c1 - 1
    else
      MerlinPy merlin.vim_type_reset()
      let init = 1
    endif
  else
    let init = 1
  endif

  let kill_loop = 0

  if init
    let max_count = max_count - 1
    MerlinPy vim.command("let w:visual_enclosing = %s" % merlin.vim_type_enclosing())
    if empty(w:visual_enclosing)
      return
    endif
    let [l1, c1, l2, c2] = s:GetVisualEnclosing()
    if l1 ==# l2
      let selection = getline(l1)
      let expr = strpart(selection, c1, c2 - c1)
      if s:IsLeaf(expr)
        " Selecting a word-like thing can be done with M or a standard
        " vim text object. We skip over it so that the initial selection
        " captures a bigger expression.
        let max_count = max_count + 1
        let kill_loop = kill_loop + 1
      endif
    endif
  endif

  while max_count ># 0 && kill_loop <# 5
    let max_count = max_count - 1
    let prev_l1 = l1
    let prev_l2 = l2
    let prev_c1 = c1
    let prev_c2 = c2
    MerlinPy vim.command("let w:visual_enclosing = %s" % merlin.vim_next_enclosing())
    if empty(w:visual_enclosing)
      break
    endif
    let [l1, c1, l2, c2] = s:GetVisualEnclosing()
    if l1 ==# l2
      let selection = getline(l1)
      let expr = strpart(selection, c1, c2 - c1)
      if s:IsLeaf(expr)
        " Selecting a word-like thing can be done with M or a standard
        " vim text object. We skip over it so that the initial selection
        " captures a bigger expression.
        let max_count = max_count + 1
        let kill_loop = kill_loop + 1
      endif
    elseif prev_l1 ==# l1 && prev_c1 ==# c1 && prev_l2 ==# l2 && prev_c2 ==# c2
      " Merlin doesn't always expand the selection: When the argument under
      " the cursor is polymorphic, it will show it twice (with and without
      " the type instantiation.) We don't want that for selections, so we
      " expand it one more time.
      let max_count = max_count + 1
      " And sometimes, it's just not possible to expand more.
      let kill_loop = kill_loop + 1
    else
      let kill_loop = 0
    endif
  endwhile
  
  if !(exists('w:l1') && w:l1 ==# l1 && w:c1 ==# c1 && w:l2 ==# l2 && w:c2 ==# c2)
    let w:l1 = l1
    let w:c1 = c1 + 1
    let w:l2 = l2
    let w:c2 = c2
  endif

endfunction

function! merlin_visual#ShrinkEnclosing(mode)
  let max_count = max([1, v:count])
  let [l1, c1] = getpos("'<")[1:2]
  let [l2, c2] = getpos("'>")[1:2]

  let init = 0

  if a:mode ==# 'v' && exists('w:l1') && w:l1 !=# -1
    if w:l1 ==# l1 && abs(w:c1 - c1) <=# 1 && w:l2 ==# l2 && abs(w:c2 - c2) <=# 1
    else
      MerlinPy merlin.vim_type_reset()
      let init = 1
    endif
  else
    let init = 1
  endif

  if init ==# 1
    MerlinPy vim.command("let w:visual_enclosing = %s" % merlin.vim_type_enclosing())
    if empty(w:visual_enclosing)
      return
    endif
    let [l1, c1, l2, c2] = s:GetVisualEnclosing()
  endif

  while max_count ># 0
    let max_count = max_count - 1
    MerlinPy vim.command("let w:visual_enclosing = %s" % merlin.vim_prev_enclosing())
    if empty(w:visual_enclosing)
      break
    endif
    let [l1, c1, l2, c2] = s:GetVisualEnclosing()
  endwhile
  
  if !(exists('w:l1') && w:l1 ==# l1 && w:c1 ==# c1 && w:l2 ==# l2 && w:c2 ==# c2)
    let w:l1 = l1
    let w:c1 = c1 + 1
    let w:l2 = l2
    let w:c2 = c2
  endif
endfunction

function! merlin_visual#GrowLeftSpaces()
  while w:l1 ># 0
    let left = getline(w:l1)
    while w:c1 ># 1 && left[w:c1 - 2] ==# ' '
      let w:c1 = w:c1 - 1
    endwhile
    if w:l1 ># 0 && w:c1 <=# 1
      let w:l1 = w:l1 - 1
      let w:c1 = strlen(getline(w:l1)) + 1
    else
      break
    endif
  endwhile
endfunction

function! merlin_visual#GrowRightSpaces()
  let max_line = line('$')
  while w:l2 <=# max_line
    let right = getline(w:l2)
    let line_length = strlen(right)
    while w:c2 <# line_length && right[w:c2] ==# ' '
      let w:c2 = w:c2 + 1
    endwhile
    if line_length ==# 0 || w:c2 >=# line_length
      if w:l2 ==# max_line
        break
      endif
      let right = getline(w:l2 + 1)
      if strlen(right) ># 0 && right[0] !=# ' '
        break
      endif
      let w:l2 = w:l2 + 1
      let w:c2 = 0
    else
      break
    endif
  endwhile
endfunction

function! merlin_visual#ShrinkLeftSpaces()
  while 1
    let left = getline(w:l1)
    let len = strlen(left) - 1
    while w:c1 <=# len && left[w:c1 - 1] ==# ' '
      let w:c1 = w:c1 + 1
    endwhile
    if len <=# 0 || w:c1 >=# len
      let w:l1 = w:l1 + 1
      let w:c1 = 1
    else
      break
    endif
  endwhile
endfunction

function! merlin_visual#ShrinkRightSpaces()
  while 1
    let right = getline(w:l2)
    let len = strlen(right)
    while w:c2 >=# 1 && right[w:c2 - 1] ==# ' '
      let w:c2 = w:c2 - 1
    endwhile
    if len ==# 0 || w:c2 <=# 1
      let w:l2 = w:l2 - 1
      let w:c2 = strlen(getline(w:l2))
    else
      break
    endif
  endwhile
endfunction

function! merlin_visual#AroundSpaces()
  call merlin_visual#GrowLeftSpaces()
  call merlin_visual#GrowRightSpaces()

  let left = getline(w:l1)
  let l = left[w:c1 - 2]
  if l !=# '(' && l !=# '['
    let w:c1 = w:c1 + 1
  endif

  let right = getline(w:l2)
  let r = left[w:c2]
  if r !=# ')' && r !=# ']' && w:c2 >=# 1
    let w:c2 = w:c2 - 1
  endif
endfunction

function! merlin_visual#InsideSpaces()
  while 1
    call merlin_visual#ShrinkLeftSpaces()
    call merlin_visual#ShrinkRightSpaces()

    let left = getline(w:l1)
    let right = getline(w:l2)
    let l = left[w:c1 - 1]
    let r = right[w:c2 - 1]
    if l ==# '(' && r ==# ')'
      let w:c1 = w:c1 + 1
      let w:c2 = w:c2 - 1
    elseif strpart(left, w:c1 - 1, 5) ==# "begin" && strpart(right, w:c2 - 3, 3) ==# "end"
      let w:c1 = w:c1 + 4
      let w:c2 = w:c2 - 3
    elseif strpart(left, w:c1 - 1, 6) ==# "struct" && strpart(right, w:c2 - 3, 3) ==# "end"
      let w:c1 = w:c1 + 5
      let w:c2 = w:c2 - 3
    else
      break
    endif
  endwhile
endfunction

function! merlin_visual#Grow(mode)
  call merlin_visual#GrowEnclosing(a:mode)
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction

function! merlin_visual#GrowAround(mode)
  call merlin_visual#GrowEnclosing(a:mode)
  call merlin_visual#AroundSpaces()
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction

function! merlin_visual#GrowInside(mode)
  call merlin_visual#GrowEnclosing(a:mode)
  call merlin_visual#InsideSpaces()
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction

function! merlin_visual#Shrink(mode)
  call merlin_visual#ShrinkEnclosing(a:mode)
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction

function! merlin_visual#ShrinkAround(mode)
  call merlin_visual#ShrinkEnclosing(a:mode)
  call merlin_visual#AroundSpaces()
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction

function! merlin_visual#ShrinkInside(mode)
  call merlin_visual#ShrinkEnclosing(a:mode)
  call merlin_visual#InsideSpaces()
  call merlin#setVisualSelection([w:l1,w:c1],[w:l2,w:c2])
endfunction
