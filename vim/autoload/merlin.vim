if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

if !exists("g:merlin_ignore_warnings")
    " strings are ugly, but at least I'm sure it's not converted in some weird
    " value when passing it to python
    let g:merlin_ignore_warnings = "false"
endif

let s:current_dir=expand("<sfile>:p:h")
py import vim
py sys.path.insert(0, vim.eval("s:current_dir"))
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
  py vim.command("e "+ merlin.vim_which(vim.eval("a:file"), vim.eval("a:ext")))
endfunction

function! merlin#Path(var,path)
python <<EOF
if vim.eval("a:path") == "":
  for path in merlin.send_command("path","list", vim.eval("a:var")):
    if path != "":
      print path
else:
  path = vim.eval("fnamemodify(a:path,':p')")
  print path
  merlin.send_command("path", "add", vim.eval("a:var"), vim.eval("a:path"))
merlin.vim_reload()
EOF
endfunction

function! merlin#PackageList(ArgLead, CmdLine, CursorPos)
  let l:pkgs = []
  py merlin.vim_find_list("l:pkgs")
  return join(l:pkgs, "\n")
endfunction

function! merlin#MLList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  py merlin.vim_which_ext(".ml", "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#MLIList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  py merlin.vim_which_ext(".mli", "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#Use(...)
  py merlin.vim_use(*vim.eval("a:000"))
  py merlin.vim_reload()
endfunction

function! merlin#RawCommand(...)
  py print send_command(*vim.eval("a:000"))
endfunction

function! merlin#TypeOf(expr)
  py merlin.vim_type(expr=vim.eval("a:expr"))
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

function! merlin#TypeEnclosing(expr)
  call merlin#StopHighlight()
  py merlin.vim_type_enclosing("w:enclosing_zone",expr=vim.eval("a:expr"))
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
  if v:version < 703
    return l:props
  else
    return {'words': l:props, 'refresh': 'always'}
  endif
endfunction

function! merlin#SyntasticGetLocList()
  let l:errors = []
  if expand('%:e') == 'ml'
    py merlin.sync_full_buffer()
    py merlin.vim_loclist("l:errors", "g:merlin_ignore_warnings")
  endif
  return l:errors 
endfunction

function! merlin#Restart()
  py merlin.vim_restart()
endfunction

function! merlin#Reload()
  py if merlin.vim_is_loaded(): merlin.vim_reload()
endfunction

function! merlin#ReloadBuffer()
  py merlin.vim_reload_buffer()
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

function! merlin#Phrase()
  let [l1, c1] = getpos("'<")[1:2]
  let [l2, c2] = getpos("'>")[1:2]
  py merlin.vim_selectphrase("l1","c1","l2","c2")
  call merlin#setVisualSelection([l1,c1],[l2,c2])
endfunction

function! merlin#Register()
  " Deprecated, use TypeEnclosing
  command! -buffer -nargs=0 TypeOf call merlin#TypeOf(merlin#WordUnderCursor())
  command! -buffer -nargs=0 TypeEnclosing call merlin#TypeEnclosing(merlin#WordUnderCursor())
  command! -buffer -nargs=0 GrowEnclosing call merlin#GrowEnclosing()
  command! -buffer -nargs=0 ShrinkEnclosing call merlin#ShrinkEnclosing()
  command! -buffer -range -nargs=0 TypeOfSel call merlin#TypeOfSel()
  command! -buffer -nargs=? -complete=dir SourcePath call merlin#Path("source", <q-args>)
  command! -buffer -nargs=? -complete=dir BuildPath  call merlin#Path("build", <q-args>)
  command! -buffer -nargs=0 Reload       call merlin#Reload()
  " Used only to debug synchronization, do not expose to end-user
  "command! -buffer -nargs=0 ReloadBuffer call merlin#ReloadBuffer()
  command! -buffer -complete=custom,merlin#PackageList -nargs=* Use call merlin#Use(<f-args>)
  command! -buffer -nargs=0 LoadProject call merlin#LoadProject()
  command! -buffer -nargs=0 EchoDotMerlin call merlin#EchoDotMerlin()
  setlocal omnifunc=merlin#Complete
  map <buffer> <LocalLeader>t :TypeEnclosing<return>
  map <buffer> <LocalLeader>n :GrowEnclosing<return>
  map <buffer> <LocalLeader>p :ShrinkEnclosing<return>
  vmap <buffer> <LocalLeader>t :TypeOfSel<return>
  vmap <buffer> <TAB> :call merlin#Phrase()<return>
endfunction

function! merlin#LoadProject()
  py merlin.load_project(vim.eval("expand('%:p:h')"))
endfunction

function! merlin#EchoDotMerlin()
  if exists("b:dotmerlin")
    echom "Using .merlin: " . b:dotmerlin
  else
    echo "No .merlin found"
  endif

endfunction

command! -nargs=1 -complete=custom,merlin#MLList ML call merlin#FindFile("ml",<f-args>)
command! -nargs=1 -complete=custom,merlin#MLIList MLI call merlin#FindFile("mli",<f-args>)
