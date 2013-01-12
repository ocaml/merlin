if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

python <<EOF
import vim
import json
import os
import pexpect

class spawn(pexpect.spawn):
  def __init(self, command):
    super(spawn, self).__init__(command)
  def send(self, s):
    r,w,e = self.__select([self.child_fd], [self.child_fd], [])
    while True:
      r,w,e = self.__select([self.child_fd], [self.child_fd], [])
      if w:
        break
      self.buffer = self.buffer + os.read(self.child_fd, 16384)
    return pexpect.spawn.send(self,s)
  def read_nonblocking(self, size = 1, timeout = -1):
    try:
      return super(spawn, self).read_nonblocking(size, timeout)
    except pexpect.EOF, e:
      raise pexpect.TIMEOUT ('Ugly workaround pexpect bug')

outliner = spawn('outliner')
outliner.delaybeforesend = 0
outliner.setecho(False)
outliner.expect('> ')

def split(input, size):
	return [input[start:start+size] for start in range(0, len(input), size)]

def send_text(content):
  for chunk in split(content, 16384):
    outliner.sendline(chunk)
  outliner.sendeof()
  outliner.expect('> ')
  return outliner.before

def send_command(cmd, arg=None):
  if arg != None:
    cmd = cmd + ' ' + json.dumps(json.dumps(arg))
  line = send_text(cmd)
  print line
  return json.loads(line)

def reset_buffer():
  return send_command("#reset")

def find_file(name):
  return send_command('#which', name)

def seek_current():
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  effective_pos = send_command("#seek", {'line' : to_line, 'col': to_col})
  line, col = effective_pos['line'], effective_pos['col']
  # pexpect sucks
  send_text(cb[line-1][col:] + "\n" + "\n".join(cb[line:to_line-1]))

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
