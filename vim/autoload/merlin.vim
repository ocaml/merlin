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
  py merlin.vim_type_expr_cursor(vim.eval("a:expr"))
endfunction

function! merlin#TypeOfSel()
  call merlin#TypeOf(s:get_visual_selection())
endfunction

function! merlin#Complete(findstart,base)
  if a:findstart
    " Locate the start of the item, including ".", "->" and "[...]".
    let line = getline('.')
    let start = col('.') - 1
    let lastword = -1
    while start > 0
      if line[start - 1] =~ '\w'
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
  return {'words': l:props, 'refresh':'always'}
endfunction

function! merlin#SyntasticGetLocList()
  let l:errors = []
  py merlin.sync_full_buffer()
  py merlin.vim_loclist("l:errors", "g:merlin_ignore_warnings")
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

function! merlin#Register()
  command! -buffer -nargs=0 TypeOf call merlin#TypeOf(substitute(substitute(expand("<cWORD>"),"[;:),]*$","",""), "^[;:(,]*", "", ""))
  command! -buffer -nargs=0 TypeCursor py merlin.vim_type_cursor()
  command! -buffer -range -nargs=0 TypeOfSel call merlin#TypeOfSel()
  command! -buffer -nargs=? -complete=dir SourcePath call merlin#Path("source", <q-args>)
  command! -buffer -nargs=? -complete=dir BuildPath  call merlin#Path("build", <q-args>)
  command! -buffer -nargs=0 Reload       call merlin#Reload()
  command! -buffer -nargs=0 ReloadBuffer call merlin#ReloadBuffer()
  command! -buffer -complete=custom,merlin#PackageList -nargs=* Use call merlin#Use(<f-args>)
  setlocal omnifunc=merlin#Complete
  map <buffer> <LocalLeader>t :TypeOf<return>
  vmap <buffer> <LocalLeader>t :TypeOfSel<return>
endfunction

function! merlin#LoadProject()
  py merlin.load_project(vim.eval("expand('%:p:h')"))
endfunction

command! -nargs=1 -complete=custom,merlin#MLList ML call merlin#FindFile("ml",<f-args>)
command! -nargs=1 -complete=custom,merlin#MLIList MLI call merlin#FindFile("mli",<f-args>)
