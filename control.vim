if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

python <<EOF
import subprocess
import json
import vim

outliner = subprocess.Popen(["outliner"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)

def send_command(*cmd):
  json.dump(cmd, outliner.stdin)
  line = outliner.stdout.readline()
  return json.loads(line)

def reset_buffer():
  return send_command("reset")

def find_file(name):
  return send_command('which', name)

def seek_current():
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  effective_pos = send_command("seek", "position", {'line' : to_line, 'col': to_col})
  #print effective_pos
  position = effective_pos[1]
  line, col = position['line'], position['col']
  send_command("tell", cb[line-1][col:] + "\n" + "\n".join(cb[line:to_line-1]) + "\n")

# [HACK] Don't know how to hook into vim to detect modified lines:
# keep a shadow cache of synced lines

cache = list()
def sync_buffer():
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  # hack : append the height of the window to parse definitions around cursor
  to_line = min(to_line + cw.height, len(cb))

  line = 0
  for line in range(0,min(to_line-1,len(cache))):
    if cb[line] != cache[line]:
      break
  if line == 0:
    send_command("reset")
    cache[:to_line-1] = cb[:to_line-1]
    send_command("tell", "\n".join(cb[:to_line-1]) + "\n")
  else:
    line = line + 1

    effective_pos = send_command("seek", "position", {'line' : line, 'col': 0})
    position = effective_pos[1]
    line, col = position['line'], position['col']
    send_command("tell", cb[line-1][col:] + "\n" + "\n".join(cb[line:to_line-1]) + "\n")

    del cache[line-1:]
    cache[line-1:to_line-1] = cb[line-1:to_line-1]

  # Now we are synced, come back to environment around cursor
  seek_current()
  # Take maximum environment without leaving current module
  send_command("seek", "maximize_scope")

reset_buffer()
EOF

function! Get_visual_selection()
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - 1]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

function! FindFile(ext,file)
  python <<EOF
vim.command("e "+ find_file(vim.eval("a:file") + "." + vim.eval("a:ext")))
EOF
endfunction

function! OLinerPath(var,path)
  python <<EOF
path = vim.eval("a:path")
#send_command('#cd', vim.eval("getcwd()"))
if path == "":
  for path in send_command(vim.eval("a:var"), "list"):
    if path != "":
      print path
else:
  send_command(vim.eval("a:var"), ["add", vim.eval("a:path")])
EOF
endfunction

function! TypeOf(expr)
  python <<EOF
sync_buffer()
expr = vim.eval("a:expr")
ty = send_command("typeof", expr)
print (expr + " : " + ty[1])
EOF
endfunction

function! TypeOfSel()
  call TypeOf(Get_visual_selection())
endfunction

command! -nargs=1 ML call FindFile("ml",<q-args>)
command! -nargs=1 MLI call FindFile("mli",<q-args>)
command! -nargs=0 TypeOf call TypeOf(substitute(substitute(expand("<cWORD>"),")*$","",""), "^(*", "", ""))
command! -range -nargs=0 TypeOfSel call TypeOfSel()
command! -nargs=* OLinerSourcePath call OLinerPath("source_path", <q-args>)
command! -nargs=* OLinerBuildPath call OLinerPath("build_path", <q-args>)

map <LocalLeader>t :TypeOf
vmap <LocalLeader>t :TypeOfSel
