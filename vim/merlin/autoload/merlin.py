import subprocess
import json
import vim
import re
import os
import sys

import vimbufsync
vimbufsync.check_version("0.1.0",who="merlin")

enclosing_types = [] # nothing to see here
current_enclosing = -1
atom_bound = re.compile('[a-z_0-9A-Z\'`.]')
re_wspaces = re.compile("[\n ]+")
re_shorten_start = re.compile("^(val|external).* : *")
re_shorten_end = re.compile(" = .*$")

######## ERROR MANAGEMENT

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

def try_print_error(e, msg=None):
  try:
    raise e
  except Error as e:
    if msg: print(msg)
    else: print(e.value['message'])
  except Exception as e:
    # Always print to stdout
    # vim try to be 'smart' and prepend a backtrace when writing to stderr
    # WTF?!
    if msg: print (msg)
    else:
      msg = str(e)
      if re.search('Not_found',msg):
        print ("error: Not found")
        return None
      elif re.search('Cmi_format.Error', msg):
        if vim.eval('exists("b:merlin_incompatible_version")') == '0':
          vim.command('let b:merlin_incompatible_version = 1')
          print ("The version of merlin you're using doesn't support this version of ocaml")
        return None
      print (msg)

def vim_encoding():
  return vim.eval("&fileencoding") or vim.eval("&encoding") or "ascii"

def catch_and_print(f, msg=None):
  try:
    return f()
  except MerlinExc as e:
    try_print_error(e, msg=msg)

######## PROCESS MANAGEMENT

class MerlinProcess:
  def __init__(self, path=None, env=None):
    self.mainpipe = None
    self.saved_sync = None
    self.path = path
    self.env = env

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
      if self.path:
        path = self.path
      else:
        path = vim.eval("merlin#FindBinary()")
      cmd = [path,"-ignore-sigint"]
      if self.env:
        env = self.env
      else:
        env = os.environ
      self.mainpipe = subprocess.Popen(
              cmd,
              stdin=subprocess.PIPE,
              stdout=subprocess.PIPE,
              stderr=None,
              env=env
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

# MULTI-PROCESS
merlin_processes = {}
def merlin_process():
  global merlin_processes
  instance = vim.eval("merlin#SelectBinary()")
  if not instance in merlin_processes:
    env = os.environ
    if vim.eval("exists('b:merlin_path')") == '1':
      path = vim.eval("b:merlin_path")
    else:
      path = instance
    if vim.eval("exists('b:merlin_env')") == '1':
      env = env.copy()
      newenv = vim.eval("b:merlin_env")
      for key in newenv:
        env[key] = newenv[key]
    merlin_processes[instance] = MerlinProcess(path=path, env=env)
  return merlin_processes[instance]

# MONO-PROCESS
#merlin_processes = None
#def merlin_process():
#  global merlin_processes
#  if not merlin_processes:
#    merlin_processes = MerlinProcess()
#  return merlin_processes

def command(*cmd):
  return merlin_process().command(*cmd)

def dump(*cmd):
  print(json.dumps(command('dump', *cmd)))

def dump_to_file(path, *cmd):
  f = open(path, 'w')
  j = command('dump', *cmd)
  f.write(json.dumps(j, indent=4, separators=(',', ': ')))

def dump_at_cursor(*cmd):
  line, col = vim.current.window.cursor
  command_seek("exact", line, col)
  dump(*cmd)

def uniq(seq):
  seen = set()
  seen_add = seen.add
  return [ x for x in seq if not (x in seen or seen_add(x))]

def shorten_desc(prop):
  if prop['kind'] == 'Value':
    return re.sub(re_shorten_end, "",
            re.sub(re_shorten_start, ": ", prop['desc']))
  else:
    return prop['desc']

def vim_is_set(name):
  return not (vim.eval('exists("%s") && %s' % (name,name)) in ["", "0"])

######## BASIC COMMANDS

def parse_position(pos):
  position = pos['cursor']
  marker = pos['marker']
  return (position['line'], position['col'], marker)

def display_load_failures(result):
  if 'failures' in result:
      for failure in result['failures']:
          print(failure)
  return result['result']

def command_tell(content):
  content = "\n".join(content) + "\n"
  return parse_position(command("tell", "source", content))

def command_find_use(*packages):
  result = catch_and_print(lambda: command('find', 'use', packages))
  return display_load_failures(result)

def command_reset(kind="auto",name=None):
  global saved_sync
  if name: r = command("reset",kind,name)
  else:    r = command("reset",kind)
  saved_sync = None
  return r

def command_seek(mtd,line,col):
  return parse_position(command("seek", mtd, {'line' : line, 'col': col}))

def command_complete_cursor(base,line,col):
  return command("complete", "prefix", base, "at", {'line' : line, 'col': col})

def command_locate(path, line, col):
  try:
    choice = vim.eval('g:merlin_locate_preference')
    if line is None or col is None:
        return command("locate", path, choice)
    else:
        pos_or_err = command("locate", path, choice, "at", {'line': line, 'col': col})
    if not isinstance(pos_or_err, dict):
      print(pos_or_err)
    else:
      l = pos_or_err['pos']['line']
      c = pos_or_err['pos']['col']
      split_method = vim.eval('g:merlin_split_method')
      # save the current position in the jump list
      vim.command("normal! m'")
      if "file" in pos_or_err:
          if split_method == "never":
              vim.command(":keepjumps e %s" % pos_or_err['file'])
          elif "tab" in split_method:
              if "always" in split_method:
                  vim.command(":keepjumps tab split %s" % pos_or_err['file'])
              else:
                  vim.command(":keepjumps tab drop %s" % pos_or_err['file'])
          elif "vertical" in split_method:
              vim.command(":keepjumps vsplit %s" % pos_or_err['file'])
          else:
              vim.command(":keepjumps split %s" % pos_or_err['file'])
      elif "always" in split_method:
          if "tab" in split_method:
              vim.command(":tab split")
          elif "vertical" in split_method:
              vim.command(":vsplit")
          else:
              vim.command(":split")
      # TODO: move the cursor using vimscript, so we can :keepjumps?
      vim.current.window.cursor = (l, c)
  except MerlinExc as e:
    try_print_error(e)

def command_occurrences(line, col):
  try:
    lst_or_err = command("occurrences", "ident", "at", {'line':line, 'col':col})
    if not isinstance(lst_or_err, list):
      print(lst_or_err)
    else:
      return lst_or_err
  except MerlinExc as e:
    try_print_error(e)

######## BUFFER SYNCHRONIZATION

def acquire_buffer(force=False):
  if not force and vim.eval('exists("b:dotmerlin")') == '0':
    return False

  process = merlin_process()
  saved_sync = process.saved_sync
  curr_sync = vimbufsync.sync()

  if saved_sync and curr_sync.bufnr() == saved_sync.bufnr():
    return False
  else:
    command_reset(name=vim.eval("expand('%:p')"))
    process.saved_sync = curr_sync
    return True

def sync_buffer_to_(to_line, to_col, skip_marker=False):
  process = merlin_process()
  saved_sync = process.saved_sync

  cb = vim.current.buffer
  max_line = len(cb)
  end_line = min(to_line, max_line)

  if not acquire_buffer(force=True):
    if saved_sync:
      line, col = min(saved_sync.pos(), (to_line, to_col))
    else:
      line, col = to_line, to_col
    col = 0
    command_seek("exact", line, col)

  line, col, _ = parse_position(command("tell", "start"))

  # Send prefix content
  if line <= end_line:
    rest    = cb[line-1][col:]
    content = cb[line:end_line]
    content.insert(0, rest)
    encoding = vim_encoding()
    content = map(lambda str: str.decode(encoding), content)
    command_tell(content)

  # put marker
  _, _, marker = parse_position(command("tell","marker"))

  # satisfy marker
  while marker and (end_line < max_line):
    next_end = min(max_line,end_line + 50)
    _, _, marker = command_tell(cb[end_line:next_end])
    end_line = next_end

  # put eof if marker still on stack at max_line
  if marker: command("tell","eof")
  if not skip_marker: command("seek","marker")

def sync_buffer_to(to_line, to_col, skip_marker=False):
  return catch_and_print(lambda: sync_buffer_to_(to_line, to_col, skip_marker=skip_marker))

def sync_buffer():
  to_line, to_col = vim.current.window.cursor
  sync_buffer_to(to_line, to_col)

def sync_full_buffer():
  sync_buffer_to(len(vim.current.buffer),0,skip_marker=True)

######## VIM FRONTEND

# Spawn a fresh new process
def vim_restart():
  merlin_process().restart()
  path = vim.eval("expand('%:p')")
  setup_merlin(path)

# Reload changed cmi files then retype all definitions
def vim_reload():
  return command("refresh")

# Complete
def vim_complete_cursor(base, vimvar):
  vim.command("let %s = []" % vimvar)
  line, col = vim.current.window.cursor
  prep = lambda str: re.sub(re_wspaces, " ", str).replace("'", "''")
  if vim_is_set("g:merlin_completion_short"):
      desc = shorten_desc
  else:
      desc = lambda prop: prop['desc']
  try:
    completions = command_complete_cursor(base,line,col)
    if completions['context'] and completions['context'][0] == 'application':
      app = completions['context'][1]
      if vim_is_set("g:merlin_completion_argtype") and (not base or atom_bound.match(base[0])):
        vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s', 'empty':1}" %
                (prep(base),prep(app['argument_type']),'',':'))
        vim.command("call add(%s, l:tmp)" % vimvar)
      for label in app['labels']:
        name = label['name']
        if not name.startswith(base): name = name.replace("?","~")
        if name.startswith(base):
          vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
                  (prep(name),prep(label['name'] + ':' + label['type']),'','~'))
          vim.command("call add(%s, l:tmp)" % vimvar)
    for prop in completions['entries']:
      vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
        (prep(prop['name']),prep(desc(prop)),prep(prop['info']),prep(prop['kind'][:1])))
      vim.command("call add(%s, l:tmp)" % vimvar)
  except MerlinExc as e:
    try_print_error(e)

def vim_expand_prefix(base, vimvar):
  sync_buffer()
  vim.command("let %s = []" % vimvar)
  line, col = vim.current.window.cursor
  try:
    l = command("expand", "prefix", base, "at", {'line' : line, 'col': col})
    l = l['entries']
    l = map(lambda prop: prop['name'], l)
    l = uniq(sorted(l))
    for prop in l:
      name = prop.replace("'", "''")
      vim.command("call add(%s, '%s')" % (vimvar, name))
  except MerlinExc as e:
    try_print_error(e)

# Error listing
def vim_loclist(vimvar, ignore_warnings):
  vim.command("let %s = []" % vimvar)
  errors = command("errors")
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

# Findlib Package
def vim_findlib_list(vimvar):
  pkgs = command('find', 'list')
  vim.command("let %s = []" % vimvar)
  for pkg in pkgs:
    vim.command("call add(%s, '%s')" % (vimvar, pkg))

def vim_findlib_use(*args):
  return command_find_use(*args)

# Locate
def vim_locate_at_cursor(path):
  line, col = vim.current.window.cursor
  sync_full_buffer()
  command_locate(path, line, col)

def vim_locate_under_cursor():
  vim_locate_at_cursor(None)

# Occurrences
def vim_occurrences(vimvar):
  vim.command("let %s = []" % vimvar)
  line, col = vim.current.window.cursor
  sync_full_buffer()
  lst = command_occurrences(line, col)
  lst = map(lambda x: x['start'], lst)
  bufnr = vim.current.buffer.number
  nr = 0
  cursorpos = 0
  for pos in lst:
    lnum = pos['line']
    lcol = pos['col']
    if (lnum, lcol) <= (line, col): cursorpos = nr
    text = vim.current.buffer[lnum - 1]
    text = text.replace("'", "''")
    vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'I','valid':1}" %
        (bufnr, lnum, lcol + 1, nr, text))
    nr = nr + 1
    vim.command("call add(%s, l:tmp)" % vimvar)
  return cursorpos + 1

def vim_occurrences_search():
  line, col = vim.current.window.cursor
  sync_full_buffer()
  lst = command_occurrences(line, col)
  result = ""
  over = ""
  start_col = 0
  for pos in lst:
    current = easy_matcher_wide(pos['start'], pos['end'])
    l1 = pos['start']['line']
    c1 = pos['start']['col']
    c2 = pos['end']['col']
    if line == l1 and col >= c1 and col <= c2:
      over = current
      start_col  = c1
    elif result == "":
      result = current
    else:
      result = result + "\\|" + current
  return "[%s, '%s', '%s']" % (start_col, over, result)

def vim_occurrences_replace(content):
  sync_full_buffer()
  line, col = vim.current.window.cursor
  lst = command_occurrences(line, col)
  lst.reverse()
  for pos in lst:
    if pos['start']['line'] == pos['end']['line']:
      mlen = pos['end']['col'] - pos['start']['col']
      matcher = make_matcher(pos['start'], pos['end'])
      query = ":%s/{0}.\\{{{1}\\}}/{2}/".format(matcher,mlen,content)
      vim.command(query)

# Expression typing
def vim_type(expr):
  to_line, to_col = vim.current.window.cursor
  cmd_at = ["at", {'line':to_line,'col':to_col}]
  sync_buffer_to(to_line,to_col)
  cmd_expr = ["expression", expr]
  try:
    cmd = ["type"] + cmd_expr + cmd_at
    ty = command(*cmd)
    res = {'type': str(ty), 'matcher': '', 'tail_info':''}
    return json.dumps(res)
  except MerlinExc as e:
    if re.search('Not_found',str(e)):
      return '{}'
    else:
      try_print_error(e)
      return '{}'

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

def vim_type_reset():
  global enclosing_types
  global current_enclosing
  enclosing_types = [] # reset
  current_enclosing = -1

def replace_buffer_portion(start, end, txt):
    encoding = vim_encoding()

    start_line = start['line'] - 1
    b = vim.current.buffer

    fst_line = b[start_line]
    lst_line = b[end['line'] - 1]

    prefix = fst_line[0:start['col']]
    suffix = lst_line[end['col']:len(lst_line)]

    del b[start_line:end['line']]

    txt = prefix.decode(encoding) + txt + suffix.decode(encoding)
    lines = txt.split('\n')
    lines.reverse()
    nb_lines = 0
    for line in lines:
        nb_lines += 1
        b[start_line:0] = [ line.encode(encoding) ]

    # Properly reindent the modified lines
    vim.current.window.cursor = (start['line'], 0)
    vim.command("call feedkeys('%d==', 'n')" % nb_lines)

def vim_case_analysis():
  global enclosing_types
  global current_enclosing

  if enclosing_types == []:
    sync_buffer()
    to_line, to_col = vim.current.window.cursor
    pos = {'line':to_line, 'col':to_col}
    try:
      enclosing_types = command("type", "enclosing", "at", pos)
      if enclosing_types != []:
        current_enclosing = 0
      else:
        atom, _, _ = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
        print("didn't manage to destruct '%s'" % atom)
        return
    except MerlinExc as e:
      try_print_error(e)
      return

  tmp = enclosing_types[current_enclosing]
  try:
    result = command("case", "analysis", "from", tmp['start'], "to", tmp['end'])
    tmp = result[0]
    txt = result[1]
    replace_buffer_portion(tmp['start'], tmp['end'], txt)
  except MerlinExc as e:
    try_print_error(e)

  vim_type_reset()

def vim_type_enclosing():
  global enclosing_types
  global current_enclosing
  vim_type_reset()
  sync_buffer()
  to_line, to_col = vim.current.window.cursor
  pos = {'line':to_line, 'col':to_col}
  # deprecated, leave merlin compute the correct identifier
  # atom, a_start, a_end = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
  # offset = to_col - a_start
  # arg = {'expr':atom, 'offset':offset}
  # enclosing_types = command("type", "enclosing", arg, pos)
  try:
    enclosing_types = command("type", "enclosing", "at", pos)
    if enclosing_types != []:
      return vim_next_enclosing()
    else:
      atom, start, stop = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
      tmp = {'start': {'line':to_line, 'col':start},
             'end':   {'line':to_line, 'col':stop }}
      tmp['matcher'] = make_matcher(tmp['start'], tmp['end'])
      tmp['atom'] = atom
      return json.dumps(tmp)
  except MerlinExc as e:
    try_print_error(e)
    return '{}'

def easy_matcher_wide(start, stop):
  startl = ""
  startc = ""
  if start['line'] > 0:
    startl = "\%{0}l".format(start['line'])
  if start['col'] > 0:
    startc = "\%{0}c".format(start['col'] + 1)
  return '{0}{1}.*\%{2}l\%{3}c'.format(startl, startc, stop['line'], stop['col'] + 1)

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

def enclosing_tail_info(record):
  if record['tail'] == 'call': return ' (* tail call *)'
  if record['tail'] == 'position': return ' (* tail position *)'
  return ''

def vim_current_enclosing():
  global enclosing_types
  global current_enclosing
  tmp = enclosing_types[current_enclosing]
  tmp['matcher'] = make_matcher(tmp['start'], tmp['end'])
  tmp['tail_info'] = enclosing_tail_info(tmp)
  return json.dumps(tmp)

def vim_next_enclosing():
  if enclosing_types != []:
    global current_enclosing
    if current_enclosing < len(enclosing_types):
        current_enclosing += 1
    if current_enclosing < len(enclosing_types):
        return vim_current_enclosing()
  return '{}'

def vim_prev_enclosing():
  if enclosing_types != []:
    global current_enclosing
    if current_enclosing >= 0:
      current_enclosing -= 1
    if current_enclosing >= 0:
      return vim_current_enclosing()
  return '{}'

# Finding files
def vim_which(name,ext):
  acquire_buffer()
  if isinstance(ext, list):
    name = map(lambda ext: name + "." + ext, ext)
  elif ext:
    name = name + "." + ext
  return command('which','path',name)

def vim_which_ext(ext,vimvar):
  acquire_buffer()
  files = command('which', 'with_ext', ext)
  vim.command("let %s = []" % vimvar)
  for f in sorted(set(files)):
    vim.command("call add(%s, '%s')" % (vimvar, f))

# Extension management
def vim_ext(enable, exts):
  acquire_buffer()
  state = enable and 'enable' or 'disable'
  result = catch_and_print(lambda: command('extension', state, exts))
  return display_load_failures(result)

def vim_ext_list(vimvar,enabled=None):
  acquire_buffer()
  if enabled == None:
    exts = command('extension','list')
  elif enabled:
    exts = command('extension','list','enabled')
  else:
    exts = command('extension','list','disabled')
  vim.command("let %s = []" % vimvar)
  for ext in exts:
    vim.command("call add(%s, '%s')" % (vimvar, ext))

# Custom flag selection
def vim_clear_flags():
  acquire_buffer()
  result = catch_and_print(lambda: command('flags', 'clear'))
  return display_load_failures(result)

def vim_add_flags(*flags):
  acquire_buffer()
  result = catch_and_print(lambda: command('flags', 'add', flags))
  return display_load_failures(result)

def vim_get_flags(var):
  acquire_buffer()
  result = catch_and_print(lambda: command('flags', 'get'))
  result = " ".join(map(" ".join, result))
  vim.command('let %s = "%s"' % (var, result.replace('"','\\"')))

# Boundaries

def min_pos(p1, p2):
    if p1['line'] < p2['line']:
        return p1
    elif p1['line'] > p2['line']:
        return p2
    elif p1['col'] <= p2['col']:
        return p1
    else:
        return p2

def max_pos(p1, p2):
    m = min_pos(p1, p2)
    return p1 if p2 == m else p2

def vim_selectphrase(l1,c1,l2,c2):
  # In some context, vim set column of '> to 2147483647 (2^31 - 1)
  # This cause the merlin json parser on 32 bit platforms to overflow
  bound = 2147483647 - 1
  vl1 = min(bound,int(vim.eval(l1)))
  vc1 = min(bound,int(vim.eval(c1)))
  vl2 = min(bound,int(vim.eval(l2)))
  vc2 = min(bound,int(vim.eval(c2)))
  sync_buffer_to(vl2,vc2)
  loc2 = command("boundary","at",{"line":vl2,"col":vc2})
  if vl2 != vl1 or vc2 != vc1:
    loc1 = command("boundary","at",{"line":vl1,"col":vc1})
  else:
    loc1 = None

  if loc2 == None:
    return

  fst = loc2[0]
  snd = loc2[1]

  if loc1 != None:
    fst = min_pos(loc1[0], loc2[0])
    snd = max_pos(loc1[1], loc2[1])
  for (var,val) in [(l1,fst['line']),(l2,snd['line']),(c1,fst['col']),(c2,snd['col'])]:
      vim.command("let %s = %d" % (var,val))

# Stuff

def setup_merlin():
  acquire_buffer(force=True)
  failures = command("project","get")
  vim.command('let b:dotmerlin=[]')
  if failures != None:
    fnames = display_load_failures(failures)
    if isinstance(fnames, list):
      vim.command('let b:dotmerlin=[%s]' % ','.join(map(lambda fname: '"'+fname+'"', fnames)))
