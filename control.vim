if !has('python')
  echo "Error: Required vim compiled with +python"
  finish
endif

python <<EOF
import subprocess
import json
import vim
import re

outliner = subprocess.Popen(["outliner"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)

######## CHANGES

changes_pattern = re.compile('(>)?\s*(\d+)\s*(\d+)\s*(\d+)\s*(.*)$')
def extract_change(match):
  return (int(match.group(3)),int(match.group(4)),match.group(5))

# find_changes(state) returns a pair (new_state, to_sync)
# Where:
#  - state is None when first called and the new_state value
#    returned by previous call after.
#  - to_sync is the list of changes since last call, or None
#    if the whole buffer have to be synced

def find_changes(previous = None):
  # compute changes as a list of match objects
  vim.command("\nredir => changes_string\nsilent changes\nredir END")
  changes = vim.eval("changes_string").split("\n")
  changes = filter(None,map(changes_pattern.match,changes))
  if len(changes) == 0:
    return (None, None)
  # find current position in change list
  position = 0
  for change in changes:
    position = position + 1
    if change.group(1):
      break
  # drop everything after cursor
  changes = changes[:position]
  # convert to canonical format (list of (line,col,contents) tuples)
  changes = list(map(extract_change,changes))
  if previous == None:
    return (changes, None)
  if changes == []:
    return (changes, [])
  marker = changes[0]
  # Find first common change in sync
  try:
    pos = previous.index(marker)
    count = 1
    maxcount = min(len(changes),len(previous) - pos)
    while count < maxcount and changes[count] == previous[pos + count]:
      count = count + 1
    return (changes, changes[count:])
  except ValueError, e:
    return (changes, None) 

def find_line(changes):
  if changes == None:
    return 0
  if changes == []:
    return None
  return reduce(min, map(lambda (lin,col,txt): lin, changes))

######## COMMUNICATION

def send_command(*cmd):
  json.dump(cmd, outliner.stdin)
  line = outliner.stdout.readline()
  return json.loads(line)

def reset_buffer():
  return send_command("reset")

def find_file(name):
  return send_command('which', name)

def seek_position(line,col):
  effective_pos = send_command("seek", "position", {'line' : line, 'col': col})
  #print effective_pos
  position = effective_pos[1]
  return (position['line'], position['col'])

last_changes = None
last_line = 0

def sync_buffer():
  global last_changes, last_line
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  # hack : append the height of the window to parse definitions near cursor
  max_line = min(to_line + 80, len(cb))

  (last_changes, sync) = find_changes(last_changes)
  #print sync
  if sync != None:
    sync_line = find_line(sync)
    if sync_line != None:
      last_line = min(last_line, sync_line)

  if last_line <= max_line:
    if last_line < 5:
      send_command("reset")
      content = cb[:max_line]
      send_command("tell", "\n".join(content))
    else:
      line, col = seek_position(last_line,0)
      rest    = cb[line-1][col:]
      content = cb[line:max_line]
      send_command("tell", rest + "\n" + "\n".join(content))
  last_line = max_line + 1

  # Now we are synced, come back to environment around cursor
  seek_position(to_line, to_col)
  # Gather maximum of definitions after cursor without leaving current module
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
  for path in send_command("path", "list", vim.eval("a:var")):
    if path != "":
      print path
else:
  send_command("path", "add", vim.eval("a:var"), vim.eval("a:path"))
EOF
endfunction

function! OLinerCommand(...)
  python <<EOF
args = vim.eval("a:000")
print send_command(*args)
EOF
endfunction

function! TypeOf(expr)
  python <<EOF
sync_buffer()
expr = vim.eval("a:expr")
ty = send_command("type", "expression", expr)
if ty[0] == "type":
  print (expr + " : " + ty[1])
elif ty[0] == "error":
  print (expr + " : " + ty[1]['message'])
else:
  print ty

EOF
endfunction

function! TypeOfSel()
  call TypeOf(Get_visual_selection())
endfunction

command! -nargs=1 ML call FindFile("ml",<q-args>)
command! -nargs=1 MLI call FindFile("mli",<q-args>)
command! -nargs=0 TypeOf call TypeOf(substitute(substitute(expand("<cWORD>"),"[;:),]*$","",""), "^[;:(,]*", "", ""))
command! -range -nargs=0 TypeOfSel call TypeOfSel()
command! -nargs=* OLiner call OLinerCommand(<f-args>)
command! -nargs=* OLinerSource call OLinerPath("source", <q-args>)
command! -nargs=* OLinerBuild call OLinerPath("build", <q-args>)

function! SyntaxCheckers_omlet_GetLocList()
  let l:errors = []
  python <<EOF
sync_buffer()

errors = send_command("errors")[1]
bufnr = vim.current.buffer.number

nr = 0
for error in errors:
  ty = 'w'
  if error['type'] == 'type':
    ty = 'e'
  vim.command("let l:error = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
    (bufnr
    ,error['start']['line']
    ,error['start']['col'] + 1
    ,nr
    ,error['message'].replace("'", "''")
    ,ty
    #'subtype':'%s', #error['in'].replace("'", "''") plus-tard peut être.
    ))
  nr = nr + 1
  vim.command("call add(l:errors, l:error)")
EOF
  return l:errors 
endfunction

map <LocalLeader>t :TypeOf
vmap <LocalLeader>t :TypeOfSel
