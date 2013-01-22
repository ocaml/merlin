import subprocess
import json
import vim
import re

######## COMMUNICATION

mainpipe = None
def restart():
  global mainpipe
  if mainpipe:
    try:
      try:
        mainpipe.terminate()
      except OSError:
        pass
      mainpipe.communicate()
    except OSError:
      pass
  try:
    mainpipe = subprocess.Popen(["orlyeh"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)
  except OSError as e:
    print("Failed to execute orlyeh. Please ensure that orlyeh binary is in path and is executable.")
    raise e
restart()

def send_command(*cmd):
  json.dump(cmd, mainpipe.stdin)
  line = mainpipe.stdout.readline()
  return json.loads(line)

######## BASIC COMMANDS

def command_reset():
  return send_command("reset")

def command_tell(content):
  return send_command("tell", content)

def command_which(name):
  return send_command('which', name)

def command_find_use(*packages):
  return send_command('find', 'use', *packages)

def command_find_list():
  return send_command('find', 'list')

def command_seek(line,col):
  effective_pos = send_command("seek", "position", {'line' : line, 'col': col})
  position = effective_pos[1]
  return (position['line'], position['col'])

def command_seek_scope():
  return send_command("seek", "maximize_scope")

def command_complete(base):
  return send_command("complete","prefix",base)

def command_report_errors():
  return send_command("errors")

######## BUFFER SYNCHRONIZATION

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
  except ValueError as e:
    return (changes, None) 

def find_line(changes):
  if changes == None:
    return 0
  if changes == []:
    return None
  return reduce(min, map((lambda (lin,col,txt): lin), changes))

last_buffer = None
last_changes = None
last_line = 0

def sync_buffer():
  global last_changes, last_line, last_buffer
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  # hack : append the height of the window to parse definitions near cursor
  max_line = min(to_line + 80, len(cb))

  # reset if necessary
  current_buffer = (cb.name, cb.number)
  if current_buffer != last_buffer:
    last_changes = None
    last_line = 0
    last_buffer = current_buffer

  (last_changes, sync) = find_changes(last_changes)
  #print sync
  if sync != None:
    sync_line = find_line(sync)
    if sync_line != None:
      last_line = min(last_line, sync_line)

  if last_line <= max_line:
    if last_line < 5:
      content = cb[:max_line]
      command_reset()
      command_tell("\n".join(content))
    else:
      line, col = command_seek(last_line,0)
      rest    = cb[line-1][col:]
      content = cb[line:max_line]
      command_tell(rest + "\n" + "\n".join(content))
  last_line = max_line + 1

  # Now we are synced, come back to environment around cursor
  command_seek(to_line, to_col)
  # Gather maximum of definitions after cursor without leaving current module
  command_seek_scope()

def vim_complete(base, vimvar):
  vim.command("let %s = []" % vimvar)
  sync_buffer()
  props = command_complete(base)
  for prop in props:
    vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
      (prop['name'].replace("'", "''")
      ,prop['desc'].replace("'", "''")
      ,prop['info'].replace("'", "''")
      ,prop['kind'][:1].upper().replace("'", "''")
      ))
    vim.command("call add(%s, l:tmp)" % vimvar)

def vim_loclist(vimvar):
  vim.command("let %s = []" % vimvar)
  errors = command_report_errors()[1]
  bufnr = vim.current.buffer.number
  nr = 0
  for error in errors:
    ty = 'w'
    if error['type'] == 'type':
      ty = 'e'
    vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
      (bufnr
      ,error['start']['line']
      ,error['start']['col'] + 1
      ,nr
      ,error['message'].replace("'", "''")
      ,ty
      ))
    nr = nr + 1
    vim.command("call add(%s, l:tmp)" % vimvar)

def vim_find_list(vimvar):
  pkgs = command_find_list()
  vim.command("let %s = []" % vimvar)
  for pkg in pkgs:
    vim.command("call add(%s, '%s')" % (vimvar, pkg))

def vim_type_expr(expr):
  sync_buffer()
  ty = send_command("type", "expression", expr)
  if ty[0] == "type":
    print (expr + " : " + ty[1])
  elif ty[0] == "error":
    print (expr + " : " + ty[1]['message'])

def vim_reload():
  command_reset()
  sync_buffer()

def vim_restart():
  restart()

def vim_which(name,ext):
  if ext:
    name = name + "." + ext
  return command_which(name)
