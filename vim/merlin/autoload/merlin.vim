if !exists('g:merlin') | let g:merlin = {} | endif | let s:c = g:merlin

if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

if !exists('g:merlin_split_method')
    let g:merlin_split_method = 'horizontal'
endif

if !exists('g:merlin_locate_preference')
    let g:merlin_locate_preference = 'ml'
endif

if !exists("g:merlin_ignore_warnings")
  " strings are ugly, but at least I'm sure it's not converted in some weird
  " value when passing it to python
  let g:merlin_ignore_warnings = "false"
endif

if !exists("g:merlin_display_occurrence_list")
  let g:merlin_display_occurrence_list = 1
endif

if !exists("g:merlin_display_error_list")
  let g:merlin_display_error_list = 1
endif

if !exists("g:merlin_close_error_list")
  let g:merlin_close_error_list = 1
endif

let s:current_dir=expand("<sfile>:p:h")
py import sys, vim
py if not vim.eval("s:current_dir") in sys.path:
\    sys.path.append(vim.eval("s:current_dir"))

call vimbufsync#init()
py import merlin

function! s:get_visual_selection()
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - 1]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

function! merlin#WordUnderCursor()
  return substitute(substitute(expand("<cWORD>"),"[;:),]*$","",""), "^[;:(,]*", "", "")
endfunction

function! merlin#FindFile(ext,file)
  py <<EOF
fname = merlin.catch_and_print(lambda: merlin.vim_which(vim.eval("a:file"), vim.eval("a:ext")))
if fname != None: vim.command("e "+ fname)
EOF
endfunction

function! merlin#Path(var,path)
  if a:path == ""
    let l:path = ""
  else
    let l:raw = (a:path =~ '^\')
    let l:path = l:raw ? substitute(a:path, '^\\\(.*\)$', '\1', '') : fnamemodify(a:path,':p')
  endif
python <<EOF
path = vim.eval("l:path")
if path == "":
  for path in merlin.command("path","list", vim.eval("a:var")):
    if path != "":
      print path
else:
  print path
  merlin.command("path", "add", vim.eval("a:var"), path)
merlin.vim_reload()
EOF
endfunction

function! merlin#PackageList(ArgLead, CmdLine, CursorPos)
  let l:pkgs = []
  py merlin.vim_findlib_list("l:pkgs")
  return join(l:pkgs, "\n")
endfunction

function! merlin#ExtEnabled(ArgLead, CmdLine, CursorPos)
  let l:exts = []
  py merlin.vim_ext_list("l:exts", enabled=True)
  return join(l:exts, "\n")
endfunction

function! merlin#ExtDisabled(ArgLead, CmdLine, CursorPos)
  let l:exts = []
  py merlin.vim_ext_list("l:exts", enabled=False)
  return join(l:exts, "\n")
endfunction

function! merlin#MLList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  py merlin.vim_which_ext([".ml",".mli"], "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#MLIList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  py merlin.vim_which_ext([".mli",".ml"], "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#ExtEnable(...)
  py merlin.vim_ext(True, vim.eval("a:000"))
  py merlin.vim_reload()
endfunction

function! merlin#ExtDisable(...)
  py merlin.vim_ext(False, vim.eval("a:000"))
  py merlin.vim_reload()
endfunction

function! merlin#Use(...)
  py merlin.vim_findlib_use(*vim.eval("a:000"))
  py merlin.vim_reload()
endfunction

function! merlin#RelevantFlags(ArgLead, CmdLine, CursorPos)
  let l:flags = [ "-rectypes", "-nostdlib", "-absname", "-debug", "-w" ]
  return join(l:flags, "\n")
endfunction

function! merlin#ClearFlags()
  py merlin.vim_clear_flags()
endfunction

function! merlin#AddFlags(...)
  py merlin.vim_add_flags(*vim.eval("a:000"))
endfunction

function! merlin#TypeOf(...)
    if (a:0 > 1)
        echoerr "TypeOf: too many arguments (expected 0 or 1)"
    elseif (a:0 == 0) || (a:1 == "")
        call merlin#StopHighlight()
        py merlin.vim_type_enclosing("w:enclosing_zone")
    else
        py merlin.vim_type(vim.eval("a:1"))
    endif
endfunction

function! merlin#TypeOfSel()
  call merlin#TypeOf(s:get_visual_selection())
endfunction

function! merlin#StopHighlight()
  if exists('w:enclosing_zone') && w:enclosing_zone != -1
    call matchdelete(w:enclosing_zone)
    let w:enclosing_zone = -1
  endif
endfunction

function! merlin#GrowEnclosing()
  call merlin#StopHighlight()
  py merlin.vim_next_enclosing("w:enclosing_zone")
endfunction

function! merlin#ShrinkEnclosing()
  call merlin#StopHighlight()
  py merlin.vim_prev_enclosing("w:enclosing_zone")
endfunction

function! merlin#Complete(findstart,base)
  if a:findstart
    " Synchronize merlin before completion, since vim modify the buffer
    " (prefix is removed)
    py merlin.sync_buffer()
    " Locate the start of the item, including ".", "->" and "[...]".
    let line = getline('.')
    let start = col('.') - 1
    let lastword = -1
    while start > 0
      if line[start - 1] =~ '\(\w\|''\)'
        let start -= 1
      elseif line[start - 1] =~ '\.'
        if lastword == -1
          let lastword = start
        endif
        let start -= 1
      else
        break
      endif
    endwhile
    " Return the column of the last word, which is going to be changed.
    " Remember the text that comes before it in s:prepended.
    if lastword == -1
      let s:prepended = ''
      return start
    endif
    let s:prepended = strpart(line, start, lastword - start)
    return lastword
  endif

  let base = s:prepended . a:base
  let l:props = []
  py merlin.vim_complete_cursor(vim.eval("base"),"l:props")

  " Workaround https://github.com/the-lambda-church/merlin/issues/223 vim 704
  return l:props
  "if v:version <= 703
  "  return l:props
  "else
  "  return {'words': l:props, 'refresh': 'always'}
  "endif
endfunction

function! merlin#Locate(...)
  if (a:0 > 1)
    echoerr "Locate: to many arguments (expected 0 or 1)"
  elseif (a:0 == 0) || (a:1 == "")
    py merlin.vim_locate_under_cursor()
  else
    py merlin.vim_locate_at_cursor(vim.eval("a:1"))
  endif
endfunction

function! merlin#Occurrences()
  let l:occurrences = []
  let l:pos = 0
  py vim.command ("let l:pos = %d" % merlin.vim_occurrences("l:occurrences"))

  if l:occurrences == []
    return
  endif

  call setloclist(0, l:occurrences)
  if g:merlin_display_occurrence_list
    lopen
    execute ":ll! " . l:pos
  endif
endfunction

function! merlin#OccurrencesRename(text)
  py merlin.vim_occurrences_replace(vim.eval("a:text"))
endfunction

function! merlin#ErrorLocList()
  let l:errors = []
  if expand('%:e') == 'ml'
    py <<EOF
try:
  merlin.sync_full_buffer()
  merlin.vim_loclist("l:errors", "g:merlin_ignore_warnings")
except merlin.MerlinException as e:
  merlin.try_print_error(e)
EOF
  endif
  return l:errors
endfunction

function! merlin#Errors()
  let l:errors = merlin#ErrorLocList()
  call setloclist(0, l:errors)
  if len(l:errors) > 0
    if g:merlin_display_error_list
      lopen
    endif
  else
    if g:merlin_close_error_list
      lclose
    endif
  endif
endfunction

function! merlin#Restart()
  py merlin.vim_restart()
endfunction

function! merlin#Reload()
  py merlin.vim_reload()
endfunction

" Copy-pasted from
" https://github.com/ngn/dotfiles/blob/master/vim/autoload/ngn/common.vim
function! merlin#setVisualSelection(a, b)
" Save existing positions of marks 'a and 'b
  let markASave = getpos("'a")
  let markBSave = getpos("'b")
" Move to a, enter visual mode, and move to b
  call setpos("'a", [0, a:a[0], a:a[1], 0])
  call setpos("'b", [0, a:b[0], a:b[1], 0])
  normal! `av`b
" Restore positions of marks 'a and 'b
  call setpos("'a", markASave)
  call setpos("'b", markBSave)
endfunction

let s:phrase_counter = 0

function! merlin#Phrase()
  if s:phrase_counter
      let s:phrase_counter = s:phrase_counter - 1
  else
    let [l1, c1] = getpos("'<")[1:2]
    let [l2, c2] = getpos("'>")[1:2]
    let s:phrase_counter = l2 - l1
    py merlin.vim_selectphrase("l1","c1","l2","c2")
    call merlin#setVisualSelection([l1,c1],[l2,c2])
  endif
endfunction

function! merlin#Register()
  command! -buffer        -nargs=? TypeOf          call merlin#TypeOf(<q-args>)
  command! -buffer -range -nargs=0 TypeOfSel       call merlin#TypeOfSel()
  command! -buffer        -nargs=? MerlinTypeOf    call merlin#TypeOf(<q-args>)
  command! -buffer -range -nargs=0 MerlinTypeOfSel call merlin#TypeOfSel()

  command! -buffer -nargs=0 ClearEnclosing        call merlin#StopHighlight()
  command! -buffer -nargs=0 GrowEnclosing         call merlin#GrowEnclosing()
  command! -buffer -nargs=0 ShrinkEnclosing       call merlin#ShrinkEnclosing()
  command! -buffer -nargs=0 MerlinClearEnclosing  call merlin#StopHighlight()
  command! -buffer -nargs=0 MerlinGrowEnclosing   call merlin#GrowEnclosing()
  command! -buffer -nargs=0 MerlinShrinkEnclosing call merlin#ShrinkEnclosing()


  command! -buffer -nargs=? Locate call merlin#Locate(<q-args>)

  command! -buffer -nargs=0 Occurrences call merlin#Occurrences()
  command! -buffer -nargs=* Rename call merlin#OccurrencesRename(<f-args>)

  command! -buffer -nargs=? -complete=dir SourcePath call merlin#Path("source", <q-args>)
  command! -buffer -nargs=? -complete=dir BuildPath  call merlin#Path("build", <q-args>)
  command! -buffer -nargs=0 Reload       call merlin#Reload()

  command! -buffer -complete=custom,merlin#PackageList   -nargs=* Use              call merlin#Use(<f-args>)
  command! -buffer -complete=custom,merlin#RelevantFlags -nargs=* AddFlags         call merlin#AddFlags(<f-args>)
  command! -buffer -nargs=0 ClearFlags   call merlin#ClearFlags()
  command! -buffer -complete=custom,merlin#ExtDisabled   -nargs=* ExtEnable        call merlin#ExtEnable(<f-args>)
  command! -buffer -complete=custom,merlin#ExtEnabled    -nargs=* ExtDisable       call merlin#ExtDisable(<f-args>)

  command! -buffer -complete=custom,merlin#PackageList   -nargs=* MerlinUse        call merlin#Use(<f-args>)
  command! -buffer -complete=custom,merlin#RelevantFlags -nargs=* MerlinAddFlags   call merlin#AddFlags(<f-args>)
  command! -buffer -nargs=0 MerlinClearFlags   call merlin#ClearFlags()
  command! -buffer -complete=custom,merlin#ExtDisabled   -nargs=* MerlinExtEnable  call merlin#ExtEnable(<f-args>)
  command! -buffer -complete=custom,merlin#ExtEnabled    -nargs=* MerlinExtDisable call merlin#ExtDisable(<f-args>)

  command! -buffer -nargs=0 ClearFlags    call merlin#ClearFlags()
  command! -buffer -nargs=0 GotoDotMerlin call merlin#GotoDotMerlin()
  command! -buffer -nargs=0 EchoDotMerlin call merlin#EchoDotMerlin()
  command! -buffer -nargs=0 MerlinClearFlags    call merlin#ClearFlags()
  command! -buffer -nargs=0 MerlinGotoDotMerlin call merlin#GotoDotMerlin()
  command! -buffer -nargs=0 MerlinEchoDotMerlin call merlin#EchoDotMerlin()

  command! -buffer -nargs=0 ErrorCheck call merlin#Errors()
  command! -buffer -nargs=0 MerlinErrorCheck call merlin#Errors()

  setlocal omnifunc=merlin#Complete
  map  <buffer> <LocalLeader>t :TypeOf<return>
  map  <buffer> <LocalLeader>n :GrowEnclosing<return>
  map  <buffer> <LocalLeader>p :ShrinkEnclosing<return>
  vmap <buffer> <LocalLeader>t :TypeOfSel<return>
  vmap <buffer> <TAB>          :call merlin#Phrase()<return>
endfunction

function! merlin#LoadProject()
  if isdirectory(expand('%:p:h'))
    py merlin.setup_merlin()
    if exists("b:dotmerlin") && exists("g:merlin_move_to_project") && g:merlin_move_to_project && len(b:dotmerlin) > 0
      execute ":lchdir " . fnamemodify(b:dotmerlin[0], ":p:h")
    endif
  endif
endfunction

function! merlin#EchoDotMerlin()
  if exists("b:dotmerlin")
    echom "Using .merlin: " . join(b:dotmerlin, ', ')
  else
    echo "No .merlin found"
  endif
endfunction

function! merlin#GotoDotMerlin()
    if exists("b:dotmerlin")
        execute ":e " . b:dotmerlin[0]
    " TODO : it's plausible to create an empty one here instead by guessing
    " where it should be located
    else
        echo "No .merlin found"
    endif
endfunction

function! merlin#FindBinary()
  if !has_key(s:c, 'ocamlmerlin_path')
    let s:choices = filter(map(['ocamlmerlin','ocamlmerlin.native'], 's:c.merlin_home."/".v:val'), 'filereadable(v:val)')
    if len(s:choices) > 0
      let s:c.ocamlmerlin_path =  s:choices[0]
    elseif executable('ocamlmerlin')
      let s:c.ocamlmerlin_path = 'ocamlmerlin'
    else
      echoe "ocamlmerlin not found!"
    endif
    unlet s:choices
  endif
  return s:c.ocamlmerlin_path
endfunction

function! merlin#SelectBinary()
  if !exists("b:merlin_binary")
    if exists("*MerlinSelectBinary")
      let b:merlin_binary = MerlinSelectBinary()
    else
      let b:merlin_binary = merlin#FindBinary()
    end
  endif
  return b:merlin_binary
endfunction

command! -nargs=1 -complete=custom,merlin#MLList  ML  call merlin#FindFile(["ml","mli"],<f-args>)
command! -nargs=1 -complete=custom,merlin#MLIList MLI call merlin#FindFile(["mli","ml"],<f-args>)

" Flush buffer and dependencies after :make
au QuickFixCmdPost * call merlin#Reload()
