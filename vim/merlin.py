import subprocess
import json
import vim
import re
import os
from collections import Counter

class Failure(Exception):
  def __init__(self, value):
      self.value = value
  def __str__(self):
    return repr(self.value)

class Exception(Exception):
  def __init__(self, value):
      self.value = value
  def __str__(self):
    return repr(self.value)

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
    mainpipe = subprocess.Popen(["ocamlmerlin"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)
  except OSError as e:
    print("Failed to execute ocamlmerlin. Please ensure that ocamlmerlin binary is in path and is executable.")
    raise e

def send_command(*cmd):
  global mainpipe
  if mainpipe == None or mainpipe.returncode != None:
    restart()
  json.dump(cmd, mainpipe.stdin)
  line = mainpipe.stdout.readline()
  result = json.loads(line)
  content = result[1:]
  if len(content) == 1:
    content = content[0]

  if result[0] == "return":
    return content
  elif result[0] == "failure":
    raise Failure(content)
  elif result[0] == "exception":
    raise Exception(content)

######## BUFFER CACHE

last_buffer = None
last_changes = None
last_line = 0
shadow_buffer = []

def clear_cache():
  global last_changes, last_line, last_buffer, shadow_buffer
  last_buffer = None
  last_changes = None
  last_line = 0
  shadow_buffer = []

######## BASIC COMMANDS

def command_reload():
  return send_command("refresh")

def command_reset():
  r = send_command("reset")
  clear_cache()
  return r

def command_tell_struct(content):
  if type(content) is list:
    return send_command("tell", "struct", "\n".join(content) + "\n")
  else:
    return send_command("tell", "struct", content)

def command_which_file(name):
  return send_command('which', 'path', name)

def command_which_with_ext(ext):
  return send_command('which', 'with_ext', ext)

def command_find_use(*packages):
  return send_command('find', 'use', *packages)

def command_find_list():
  return send_command('find', 'list')

def command_seek(line,col):
  position = send_command("seek", "position", {'line' : line, 'col': col})
  return (position['line'], position['col'])

def command_seek_scope():
  return send_command("seek", "maximize_scope")

def command_seek_end():
  return send_command("seek", "end")

def command_complete(base):
  return send_command("complete","prefix",base)

def command_report_errors():
  return send_command("errors")

######## BUFFER SYNCHRONIZATION

changes_pattern = re.compile('(>)?\s*(\d+)\s*(\d+)\s*(\d+)\s*(.*)$')
def extract_change(match):
  return (int(match.group(3)),int(match.group(4)),match.group(5))

def current_changes():
  # compute changes as a list of match objects
  vim.command("\nredir => changes_string\nsilent changes\nredir END")
  changes = vim.eval("changes_string").split("\n")
  changes = filter(None,map(changes_pattern.match,changes))
  if len(changes) == 0:
    return None
  # find current position in change list
  position = 0
  for change in changes:
    position += 1
    if change.group(1):
      break
  # drop everything after cursor
  changes = changes[:position]
  # convert to canonical format (list of (line,col,contents) tuples)
  return Counter(map(extract_change,changes))

# find_changes(state) returns a pair (new_state, to_sync)
# Where:
#  - state is None when first called and the new_state value
#    returned by previous call after.
#  - to_sync is the list of changes since last call, or None
#    if the whole buffer have to be synced

def find_changes(previous = None):
  changes = current_changes()
  if previous == None:
    return (changes, None)
  if len(changes) == 0:
    return (changes, [])

  return (changes, (changes - previous).elements())

def find_line(changes):
  if changes == None:
    return 0
  lines = set(lin for lin,col,txt in changes) 
  if lines:
    return min(lines)
  return None

def sync_buffer_to(to_line, to_col):
  global last_changes, last_line, last_buffer, shadow_buffer
  cb = vim.current.buffer
  # hack : append some lines to parse definitions near cursor
  max_line = len(cb)
  end_line = min(to_line, max_line)

  # reset if necessary
  current_buffer = (cb.name, cb.number)
  content = None
  if current_buffer == last_buffer:
    (last_changes, sync) = find_changes(last_changes)
    # find changes
    if sync != None:
      sync_line = find_line(sync)
      if sync_line != None:
        last_line = min(last_line, sync_line)
    # sync shadow buffer
    last_line = min(last_line,len(shadow_buffer), len(cb))
    # heuristic: find 3 equal lines in a row
    in_a_row = 0
    while last_line > 0 and in_a_row < 3:
      last_line -= 1
      if shadow_buffer[last_line] == cb[last_line]:
        if shadow_buffer[last_line] != "":
          in_a_row += 1
      else:
        in_a_row = 0
    last_line += 1 + in_a_row
    if last_line <= end_line:
      if last_line <= 1:
        command_reset()
        content = cb[:end_line]
        shadow_buffer = content
      else:
        line, col = command_seek(last_line,0)
        rest    = cb[line-1][col:]
        content = cb[line:end_line]
        content.insert(0, rest)
        shadow_buffer[line-1:] = cb[line-1:end_line]
  else:
    command_reset()
    (last_changes, sync) = find_changes(None)
    last_buffer = current_buffer
    content = cb[:end_line]
    shadow_buffer = content

  if content != None:
    while not command_tell_struct(content):
      if end_line < max_line:
        next_end = min(max_line,end_line + 4)
        content = cb[end_line:next_end]
        end_line = next_end
      else:
        content = None
    last_line = end_line + 1

  # Now we are synced, come back to environment around cursor
  command_seek(to_line, to_col)
  # Gather a maximum of definition after cursor without leaving current module
  command_seek_scope()

def sync_buffer():
  to_line, to_col = vim.current.window.cursor
  sync_buffer_to(to_line, to_col)

def sync_full_buffer():
  sync_buffer_to(len(vim.current.buffer),0)

def vim_complete(base, vimvar):
  vim.command("let %s = []" % vimvar)
  sync_buffer()
  props = command_complete(base)
  for prop in props:
    vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
      (prop['name'].replace("'", "''")
      ,prop['desc'].replace("\n"," ").replace("  "," ").replace("'", "''")
      ,prop['info'].replace("'", "''")
      ,prop['kind'][:1].upper().replace("'", "''")
      ))
    vim.command("call add(%s, l:tmp)" % vimvar)

def vim_loclist(vimvar):
  vim.command("let %s = []" % vimvar)
  errors = command_report_errors()
  bufnr = vim.current.buffer.number
  nr = 0
  for error in errors:
    ty = 'w'
    if error['type'] == 'type':
      ty = 'e'
    lnum = 1
    col = 1
    if error.has_key('start'):
        lnum = error['start']['line']
        lcol = error['start']['col'] + 1
    vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
        (bufnr, lnum, lcol, nr, error['message'].replace("'", "''"), ty))
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
  print (expr + " : " + ty)

def vim_type_cursor():
  to_line, to_col = vim.current.window.cursor
  sync_buffer()
  ty = send_command("type", "at", {'line':to_line,'col':to_col})
  print (ty)

# Resubmit current buffer
def vim_reload_buffer():
  command_reset()
  clear_cache()
  sync_buffer()

# Reload changed cmi files then retype all definitions
def vim_is_loaded():
  return (mainpipe != None)

def vim_reload():
  command_reload()

# Spawn a fresh new process
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
  fname = os.path.join(directory,".merlin") 
  if os.path.exists(fname):
    with open(fname,"r") as f:
      for line in f:
        split = line.split(None,1)
        if split != []:
          command = split[0]
          tail = split[1]
          if command and command[0] == '#':
            continue
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

