import subprocess
import signal
import json
import vim
import re
import os
import sys
from itertools import groupby

import vimbufsync
vimbufsync.check_version("0.1.0",who="merlin")

flags = []

enclosing_types = [] # nothing to see here
current_enclosing = -1

atom_bound = re.compile('[a-z_0-9A-Z\'`.]')

class MerlinExc(Exception):
  def __init__(self, value):
      self.value = value
  def __str__(self):
    return repr(self.value)

class Failure(MerlinExc):
  pass

class Error(MerlinExc):
  pass

class MerlinException(MerlinExc):
  pass

######## COMMUNICATION

class MerlinProcess:
  def __init__(self):
    self.mainpipe = None
    self.saved_sync = None

  def restart(self):
    if self.mainpipe:
      try:
        try:
          self.mainpipe.terminate()
        except OSError:
          pass
        self.mainpipe.communicate()
      except OSError:
        pass
    try:
      cmd = [vim.eval("merlin#FindOcamlMerlin()"),"-ignore-sigint"]
      cmd.extend(flags)
      self.mainpipe = subprocess.Popen(
              cmd,
              stdin=subprocess.PIPE,
              stdout=subprocess.PIPE,
              stderr=None,
          )
    except OSError as e:
      print("Failed starting ocamlmerlin. Please ensure that ocamlmerlin binary\
              is executable.")
      raise e

  def command(self, *cmd):
    if self.mainpipe == None or self.mainpipe.returncode != None:
      self.restart()
    json.dump(cmd, self.mainpipe.stdin)
    line = self.mainpipe.stdout.readline()
    result = json.loads(line)
    content = None
    if len(result) == 2:
      content = result[1]

    if result[0] == "return":
      return content
    elif result[0] == "failure":
      raise Failure(content)
    elif result[0] == "error":
      raise Error(content)
    elif result[0] == "exception":
      raise MerlinException(content)


merlin_processes = {}
def merlin_process():
  global merlin_processes
  name = vim.eval("exists('b:merlin_project') ? b:merlin_project : ''")
  if not name in merlin_processes:
    merlin_processes[name] = MerlinProcess()
  return merlin_processes[name]

def command(*cmd):
  return merlin_process().command(*cmd)

def dump(*cmd):
  print(command('dump', *cmd))

def try_print_error(e, msg=None):
  try:
    raise e
  except Error as e:
    if msg: print(msg)
    else:
      print(e.value['message'])
  except Exception as e:
    if msg: sys.stderr.write(msg)
    else:
      msg = str(e)
      if re.search('Not_found',msg):
        print ("error: Not found")
        return None
      elif re.search('Cmi_format.Error', msg):
        sys.stderr.write ("error: The version of merlin you're using doesn't support this version of ocaml")
        return None
      sys.stderr.write(msg)

def catch_and_print(f, msg=None):
  try:
    return f()
  except MerlinExc as e:
    try_print_error(e, msg=msg)

def path_is_recursive(path):
  for component in path:
    if isinstance(component,list):
      for x in component:
        if x == "rec":
          return True
    elif component == "rec":
      return True
  return False

def path_common(p1,p2):
  l = min(len(p1),len(p2))
  p = []
  for i in range(l):
    if p1[i] == p2[i]:
      p.append(p1[i])
    else:
      c1, c2 = c1[i], c2[i]
      l = min(len(c1),len(c2))
      c = []
      for i in range(l):
        if c1[i] == c2[i]:
          c.append(c1[i])
      p.append(c)
      return p
  return p

def parse_position(pos):
  position = pos['pos']
  path = pos['path']
  return (position['line'], position['col'], path)

######## BASIC COMMANDS

def command_reset(kind="ml",name=None):
  global saved_sync
  if name: r = command("reset",kind,name)
  else:    r = command("reset",kind)
  if name == "myocamlbuild.ml":
    command_find_use("ocamlbuild")
  saved_sync = None
  return r

def command_tell(content):
  if isinstance(content,list):
    content = "\n".join(content) + "\n"
  return parse_position(command("tell", "source", content))

def command_which_file(name):
  return command('which', 'path', name)

def command_which_with_ext(ext):
  return command('which', 'with_ext', ext)

def command_ext_list():
  return command('extension', 'list')

def command_ext_enabled():
  return command('extension', 'list', 'enabled')

def command_ext_disabled():
  return command('extension', 'list', 'disabled')

def display_load_failures(result):
  if 'failures' in result:
    print (result['failures'])
  return result['result']

def command_find_use(*packages):
  result = catch_and_print(lambda: command('find', 'use', packages))
  return display_load_failures(result)

def command_seek(mtd,line,col):
  return parse_position(command("seek", mtd, {'line' : line, 'col': col}))

def command_seek_end():
  return command("seek", "end")

def command_complete_cursor(base,line,col):
  return command("complete", "prefix", base, "at", {'line' : line, 'col': col})

def command_report_errors():
  return command("errors")

def command_locate(path, line, col):
  try:
    if line is None or col is None:
        return command("locate", path)
    else:
        pos_or_err = command("locate", path, "at", {'line': line, 'col': col})
    if not isinstance(pos_or_err, dict):
      print(pos_or_err)
    else:
      l = pos_or_err['pos']['line']
      c = pos_or_err['pos']['col']
      if "file" in pos_or_err:
        vim.command(":split %s" % pos_or_err['file'])
      vim.current.window.cursor = (l, c)
  except MerlinExc as e:
    try_print_error(e)

######## BUFFER SYNCHRONIZATION

def sync_buffer_to(to_line, to_col):
  process = merlin_process()
  saved_sync = process.saved_sync
  curr_sync = vimbufsync.sync()
  cb = vim.current.buffer
  max_line = len(cb)
  end_line = min(to_line, max_line)

  if saved_sync and curr_sync.bufnr() == saved_sync.bufnr():
    line, col = saved_sync.pos()
    line, col, path = command_seek("before", line, 0)
    if line <= end_line:
      rest    = cb[line-1][col:]
      content = cb[line:end_line]
      content.insert(0, rest)
      process.saved_sync = curr_sync
    else:
      content = None
  else:
    command_reset(name=vim.eval("expand('%:p')"))
    path = []
    content = cb[:end_line]
    process.saved_sync = curr_sync

  # Send content
  if content:
    kind = "source"
    _, _, path = command_tell(content)
    while path_is_recursive(path):
      if end_line < max_line:
        next_end = min(max_line,end_line + 50)
        _, _, p2 = command_tell(cb[end_line:next_end])
        path = path_common(path,p2)
        end_line = next_end
      else:
        break

def sync_buffer():
  to_line, to_col = vim.current.window.cursor
  sync_buffer_to(to_line, to_col)

def sync_full_buffer():
  sync_buffer_to(len(vim.current.buffer),0)

def vim_complete_cursor(base, vimvar):
  vim.command("let %s = []" % vimvar)
  line, col = vim.current.window.cursor
  wspaces = re.compile("[\n ]+")
  try:
    sync_buffer()
    props = command_complete_cursor(base,line,col)
    for prop in props:
      vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
        (prop['name'].replace("'", "''")
        ,re.sub(wspaces, " ", prop['desc']).replace("'", "''")
        ,prop['info'].replace("'", "''")
        ,prop['kind'][:1].replace("'", "''")
        ))
      vim.command("call add(%s, l:tmp)" % vimvar)
  except MerlinExc as e:
    try_print_error(e)

def vim_loclist(vimvar, ignore_warnings):
  vim.command("let %s = []" % vimvar)
  errors = command_report_errors()
  bufnr = vim.current.buffer.number
  nr = 0
  for error in errors:
    if error['type'] == 'warning' and vim.eval(ignore_warnings) == 'true':
        continue
    ty = 'w'
    if error['type'] == 'type' or error['type'] == 'parser':
      ty = 'e'
    lnum = 1
    lcol = 1
    if error.has_key('start'):
        lnum = error['start']['line']
        lcol = error['start']['col'] + 1
    vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
        (bufnr, lnum, lcol, nr, error['message'].replace("'", "''").replace("\n", " "), ty))
    nr = nr + 1
    vim.command("call add(%s, l:tmp)" % vimvar)

def vim_find_list(vimvar):
  pkgs = command('find', 'list')
  vim.command("let %s = []" % vimvar)
  for pkg in pkgs:
    vim.command("call add(%s, '%s')" % (vimvar, pkg))

def vim_type(expr,is_approx=False):
  to_line, to_col = vim.current.window.cursor
  cmd_at = ["at", {'line':to_line,'col':to_col}]
  sync_buffer_to(to_line,to_col)
  cmd_expr = ["expression", expr] if expr else []
  try:
    cmd = ["type"] + cmd_expr + cmd_at
    ty = command(*cmd)
    if isinstance(ty,dict):
      if "type" in ty: ty = ty['type']
      else: ty = str(ty)
    if is_approx: sys.stdout.write("(approx) ")
    if expr: print(expr + " : " + ty)
    else: print(ty)
  except MerlinExc as e:
    if re.search('Not_found',str(e)):
      pass
    else:
      try_print_error(e)

def vim_locate_at_cursor(path):
  line, col = vim.current.window.cursor
  sync_buffer_to(line, col)
  command_locate(path, line, col)

def vim_locate_under_cursor():
  delimiters = [' ', '\n', '=', ';', ',', '(', ')', '[', ']', '{', '}', '|', '"',"+","-","*","/" ]
  line_nb, col_nb = vim.current.window.cursor
  line = vim.current.buffer[line_nb - 1]
  start = col_nb
  stop = col_nb
  while start > 0:
    if line[start - 1] in delimiters:
        break
    else:
        start -= 1
  while stop < len(line):
    # we stop on dots because on "Foo.Ba<cursor>r.Baz.lol" I want to jump at the
    # definition of Bar, not the one of lol.
    if line[stop] in delimiters or line[stop] == '.':
        break
    else:
        stop += 1
  vim_locate_at_cursor(line[start:stop])

def bounds_of_ocaml_atom_at_pos(to_line, col):
    line = vim.current.buffer[to_line]
    start = col
    stop = col
    while start > 0:
        if atom_bound.match(line[start - 1]) is None:
            break
        else:
            start -= 1
    while stop < len(line):
        if atom_bound.match(line[stop]) is None:
            break
        else:
            stop += 1
    return (line[start:stop], start, stop)

# expr used as fallback in case type_enclosing fail
def vim_type_enclosing(vimvar,expr=None):
  global enclosing_types
  global current_enclosing
  enclosing_types = [] # reset
  current_enclosing = -1
  to_line, to_col = vim.current.window.cursor
  atom, a_start, a_end = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
  offset = to_col - a_start
  pos = {'line':to_line, 'col':to_col}
  arg = {'expr':atom, 'offset':offset}
  sync_buffer()
  try:
    enclosing_types = command("type", "enclosing", arg, pos)
    if enclosing_types != []:
      vim_next_enclosing(vimvar)
    else:
      print("didn't manage to type '%s'" % atom)
  except MerlinExc as e:
    try_print_error(e)

def easy_matcher(start, stop):
  startl = ""
  startc = ""
  if start['line'] > 0:
    startl = "\%>{0}l".format(start['line'] - 1)
  if start['col'] > 0:
    startc = "\%>{0}c".format(start['col'])
  return '{0}{1}\%<{2}l\%<{3}c'.format(startl, startc, stop['line'] + 1, stop['col'] + 1)

def hard_matcher(start, stop):
  first_start = {'line' : start['line'], 'col' : start['col']}
  first_stop =  {'line' : start['line'], 'col' : 4242}
  first_line = easy_matcher(first_start, first_stop)
  mid_start = {'line' : start['line']+1, 'col' : 0}
  mid_stop =  {'line' : stop['line']-1 , 'col' : 4242}
  middle = easy_matcher(mid_start, mid_stop)
  last_start = {'line' : stop['line'], 'col' : 0}
  last_stop =  {'line' : stop['line'], 'col' : stop['col']}
  last_line = easy_matcher(last_start, last_stop)
  return "{0}\|{1}\|{2}".format(first_line, middle, last_line)

def make_matcher(start, stop):
  if start['line'] == stop['line']:
    return easy_matcher(start, stop)
  else:
    return hard_matcher(start, stop)

def vim_next_enclosing(vimvar):
  if enclosing_types != []:
    global current_enclosing
    if current_enclosing < len(enclosing_types):
        current_enclosing += 1
    if current_enclosing < len(enclosing_types):
      tmp = enclosing_types[current_enclosing]
      matcher = make_matcher(tmp['start'], tmp['end'])
      vim.command("let {0} = matchadd('EnclosingExpr', '{1}')".format(vimvar, matcher))
      print(tmp['type'])

def vim_prev_enclosing(vimvar):
  if enclosing_types != []:
    global current_enclosing
    if current_enclosing >= 0:
      current_enclosing -= 1
    if current_enclosing >= 0:
      tmp = enclosing_types[current_enclosing]
      matcher = make_matcher(tmp['start'], tmp['end'])
      vim.command("let {0} = matchadd('EnclosingExpr', '{1}')".format(vimvar, matcher))
      print(tmp['type'])

# Resubmit current buffer
def vim_reload_buffer():
  clear_cache()
  sync_buffer()

# Reload changed cmi files then retype all definitions
def vim_reload():
  return command("refresh")

# Spawn a fresh new process
def vim_restart():
  merlin_process().restart()
  path = vim.eval("expand('%:p')")
  load_project(path, force=True)

def vim_which(name,ext):
  if ext:
    name = name + "." + ext
  return command_which_file(name)

def vim_which_ext(ext,vimvar):
  files = command_which_with_ext(ext)
  vim.command("let %s = []" % vimvar)
  for f in sorted(set(files)):
    vim.command("call add(%s, '%s')" % (vimvar, f))

def vim_use(*args):
  return command_find_use(*args)

def vim_ext(enable, exts):
  state = enable and 'enable' or 'disable'
  catch_and_print(lambda: command('extension', state, exts))

def vim_ext_list(vimvar,enabled=None):
  if enabled == None:
    exts = command_ext_list()
  elif enabled:
    exts = command_ext_enabled()
  else:
    exts = command_ext_disabled()
  vim.command("let %s = []" % vimvar)
  for ext in exts:
    vim.command("call add(%s, '%s')" % (vimvar, ext))

def vim_clear_flags():
  global flags
  flags = []
  vim_restart()

def vim_add_flags(*args):
  flags.extend(args)
  vim_restart()

def vim_selectphrase(l1,c1,l2,c2):
  # In some context, vim set column of '> to 2147483647 (2^31 - 1)
  # This cause the merlin json parser on 32 bit platforms to overflow
  bound = 2147483647 - 1
  vl1 = min(bound,int(vim.eval(l1)))
  vc1 = min(bound,int(vim.eval(c1)))
  vl2 = min(bound,int(vim.eval(l2)))
  vc2 = min(bound,int(vim.eval(c2)))
  sync_buffer_to(vl2,vc2)
  command_seek_exact(vl2,vc2)
  loc2 = command("boundary")
  if vl2 != vl1 or vc2 != vc1:
    command_seek_exact(vl1,vc1)
    loc1 = command("boundary")
  else:
    loc1 = None

  if loc2 == None:
    return

  vl1 = loc2[0]['line']
  vc1 = loc2[0]['col']
  vl2 = loc2[1]['line']
  vc2 = loc2[1]['col']
  if loc1 != None:
    vl1 = min(loc1[0]['line'], vl1)
    vc1 = min(loc1[0]['col'], vc1)
    vl2 = max(loc1[1]['line'], vl2)
    vc2 = max(loc1[1]['col'], vc2)
  for (var,val) in [(l1,vl1),(l2,vl2),(c1,vc1),(c2,vc2)]:
    vim.command("let %s = %d" % (var,val))

def load_project(directory,force=False):
  cmd = [vim.eval("merlin#FindOcamlMerlin()"), "-project-find", directory]
  process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  name = process.communicate()[0].strip()
  if not force:
    if name == vim.eval("b:merlin_project"): return
  vim.command("let b:merlin_project = '%s'" % name)
  failures = catch_and_print(lambda: command("project","find",directory))
  fnames = display_load_failures(failures)
  if isinstance(fnames, list):
    vim.command('let b:dotmerlin=[%s]' % ','.join(map(lambda fname: '"'+fname+'"', fnames)))
  sync_buffer_to(1, 0)
