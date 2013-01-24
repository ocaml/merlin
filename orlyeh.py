import subprocess
import json
import vim
import re
import os

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

def send_command(*cmd):
  global mainpipe
  if mainpipe == None or mainpipe.returncode != None:
    restart()
  json.dump(cmd, mainpipe.stdin)
  line = mainpipe.stdout.readline()
  return json.loads(line)

######## BUFFER CACHE

last_buffer = None
last_changes = None
last_line = 0

def clear_cache():
  global last_changes, last_line, last_buffer
  last_buffer = None
  last_changes = None
  last_line = 0

######## BASIC COMMANDS

def command_reset():
  r = send_command("reset")
  clear_cache()
  return r

def command_tell(content):
  return send_command("tell", content)

def command_which_file(name):
  return send_command('which', 'path', name)

def command_which_with_ext(ext):
  return send_command('which', 'with_ext', ext)

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

def sync_buffer():
  global last_changes, last_line, last_buffer
  cb = vim.current.buffer
  cw = vim.current.window
  to_line, to_col = cw.cursor
  # hack : append the height of the window to parse definitions near cursor
  max_line = min(to_line + 80, len(cb))

  # reset if necessary
  current_buffer = (cb.name, cb.number)
  if current_buffer == last_buffer:
    (last_changes, sync) = find_changes(last_changes)
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
  else:
    (last_changes, sync) = find_changes(None)
    last_buffer = current_buffer
    content = cb[:max_line]
    command_reset()
    command_tell("\n".join(content))

  last_line = max_line + 1

  # Now we are synced, come back to environment around cursor
  command_seek(to_line, to_col)
  # Gather a maximum of definition after cursor without leaving current module
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
  clear_cache()
  sync_buffer()

def vim_restart():
  restart()

def vim_which(name,ext):
  if ext:
    name = name + "." + ext
  return command_which_file(name)

def vim_which_ext(ext,vimvar):
  files = command_which_with_ext(ext)
  vim.command("let %s = []" % vimvar)
  for f in sorted(set(files)):
    vim.command("call add(%s, '%s')" % (vimvar, f))

def load_project(directory,maxdepth=3):
  fname = os.path.join(directory,".orlyeh") 
  if os.path.exists(fname):
    with open(fname,"r") as f:
      for line in f:
        split = line.split(None,1)
        if split != []:
          command = split[0]
          tail = split[1]
          if tail != "":
            tail = os.path.join(directory,tail)
          if command == "S":
            send_command("path","add","source",tail.strip())
          elif command == "B":
            send_command("path","add","build",tail.strip())
          elif command == "PKG":
            command_find_use(*split[1].split())
    command_reset()
  elif maxdepth > 0:
    (head, tail) = os.path.split(directory)
    if head != "":
      load_project(head,maxdepth - 1)

