if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

python <<EOF
import subprocess
import json
import vim

outliner = subprocess.Popen(["outliner"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)

def send_command(cmd, arg=None):
  if arg != None:
    cmd = [cmd,arg]
  else:
    cmd = [cmd]
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
  effective_pos = send_command("seek", {'line' : to_line, 'col': to_col})
  print effective_pos
  position = effective_pos[1]
  line, col = position['line'], position['col']
  send_command("tell", cb[line-1][col:] + "\n" + "\n".join(cb[line:to_line-1]) + "\n")

reset_buffer()
EOF

function! FindFile(ext,file)
  python <<EOF
vim.command("e "+ find_file(vim.eval("a:file") + "." + vim.eval("a:ext")))
EOF
endfunction

function! OLinerPath(var,path)
    echo a:path 
    python <<EOF
path = vim.eval("a:path")
#send_command('#cd', vim.eval("getcwd()"))
if path == "":
  for path in send_command('#' + vim.eval("a:var")):
    if path != "":
      print path
else:
  send_command('#' + vim.eval("a:var"), vim.eval("a:path"))
EOF
endfunction

command! -nargs=1 ML call FindFile("ml",<q-args>)
command! -nargs=1 MLI call FindFile("mli",<q-args>)
command! -nargs=* OLinerSourcePath call OLinerPath("source_path", <q-args>)
command! -nargs=* OLinerBuildPath call OLinerPath("build_path", <q-args>)
