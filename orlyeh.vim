if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

let s:current_dir=expand("<sfile>:p:h")
py import vim
py sys.path.insert(0, vim.eval("s:current_dir"))
py import orlyeh

function! s:get_visual_selection()
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - 1]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

function! orlyeh#FindFile(ext,file)
  py vim.command("e "+ orlyeh.vim_which(vim.eval("a:file"), vim.eval("a:ext")))
endfunction

function! orlyeh#Path(var,path)
python <<EOF
if vim.eval("a:path") == "":
  for path in orlyeh.send_command("path","list", vim.eval("a:var")):
    if path != "":
      print path
else:
  path = vim.eval("fnamemodify(a:path,':p')")
  orlyeh.send_command("path", "add", vim.eval("a:var"), vim.eval("a:path"))
orlyeh.vim_reload()
EOF
endfunction

function! orlyeh#PackageList(ArgLead, CmdLine, CursorPos)
  let l:pkgs = []
  py orlyeh.vim_find_list("l:pkgs")
  return join(l:pkgs, "\n")
endfunction

function! orlyeh#Use(...)
  py orlyeh.command_find_use(*vim.eval("a:000"))
  py orlyeh.vim_reload()
endfunction

function! orlyeh#RawCommand(...)
  py print send_command(*vim.eval("a:000"))
endfunction

function! orlyeh#TypeOf(expr)
  py orlyeh.vim_type_expr(vim.eval("a:expr"))
endfunction

function! orlyeh#TypeOfSel()
  call orlyeh#TypeOf(s:get_visual_selection())
endfunction

function! orlyeh#Complete(findstart,base)
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
  py orlyeh.vim_complete(vim.eval("base"),"l:props")
  return {'words': l:props, 'refresh':'always'}
endfunction

function! SyntaxCheckers_omlet_GetLocList()
  let l:errors = []
  py orlyeh.sync_buffer()
  py orlyeh.vim_loclist("l:errors")
  return l:errors 
endfunction

function! orlyeh#Register()
  command! -buffer -nargs=0 TypeOf call orlyeh#TypeOf(substitute(substitute(expand("<cWORD>"),"[;:),]*$","",""), "^[;:(,]*", "", ""))
  command! -buffer -range -nargs=0 TypeOfSel call orlyeh#TypeOfSel()
  command! -buffer -nargs=? -complete=dir SourcePath call orlyeh#Path("source", <q-args>)
  command! -buffer -nargs=? -complete=dir BuildPath call orlyeh#Path("build", <q-args>)
  command! -buffer -nargs=0 Reload   py orlyeh.vim_reload()
  command! -buffer -nargs=0 Restart  py orlyeh.vim_restart()
  command! -buffer -complete=custom,orlyeh#PackageList -nargs=* Use  call orlyeh#Use(<f-args>)
  setlocal omnifunc=orlyeh#Complete
  map <buffer> <LocalLeader>t :TypeOf
  vmap <buffer> <LocalLeader>t :TypeOfSel
endfunction

command! -nargs=1 ML call orlyeh#FindFile("ml",<f-args>)
command! -nargs=1 MLI call orlyeh#FindFile("mli",<f-args>)
au FileType omlet call orlyeh#Register()

