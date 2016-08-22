import subprocess
import json
import vim
import re
import os
import sys
from sys import platform

enclosing_types = [] # nothing to see here
current_enclosing = -1
atom_bound = re.compile('[a-z_0-9A-Z\'`.]')
re_wspaces = re.compile("[\n ]+")

protocol_version = 3

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

def vim_codec():
    # Vim passed incorrectly encoded strings to python2.
    # This could be worked around by manually decoding using the buffer
    # encoding.
    # However, python3 handling of unicode is a bit better, so "str()"
    # shouldn't be decoded. So we assume that vim did the right thing before.
    if sys.version_info >= (3,0):
        return ((lambda str: str), (lambda str: str))
    else:
        encoding = vim.eval("&fileencoding") or \
                   vim.eval("&encoding") or \
                   "ascii"
        return ((lambda str: str.encode(encoding)), \
                (lambda str: str.decode(encoding)))

def catch_and_print(f, msg=None):
    try:
        return f()
    except MerlinExc as e:
        try_print_error(e, msg=msg)

######## PROCESS MANAGEMENT

class MerlinProcess:
    def __init__(self, path=None, env=None):
        self.mainpipe = None
        self.path = path
        self.env = env

    def restart(self):
        vim.command("let b:merlin_tick = 0")
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
            cmd = [path,"-ignore-sigint"] + vim.eval('g:merlin_binary_flags')
            if self.env:
                env = self.env
            else:
                env = os.environ
            # As for OCaml, 64-bit Python still has sys.platform == win32
            # Note that owing to a long-standing bug in Python, stderr must be given
            # (see https://bugs.python.org/issue3905)
            if platform == "win32":
                info = subprocess.STARTUPINFO()
                info.dwFlags |= subprocess.STARTF_USESHOWWINDOW
                info.wShowWindow = subprocess.SW_HIDE
                self.mainpipe = subprocess.Popen(
                        cmd,
                        stdin=subprocess.PIPE,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        startupinfo=info,
                        universal_newlines=True,
                        env=env
                        )
            else:
                self.mainpipe = subprocess.Popen(
                        cmd,
                        stdin=subprocess.PIPE,
                        stdout=subprocess.PIPE,
                        stderr=None,
                        universal_newlines=True,
                        env=env
                        )

            # Protocol version negotiation
            json.dump(["protocol","version",protocol_version], self.mainpipe.stdin)
            self.mainpipe.stdin.flush()
            answer = json.loads(self.mainpipe.stdout.readline())
            if isinstance(answer, dict) and answer['class'] == "return":
                value = answer['value']
                if value['selected'] != protocol_version:
                    print("Unsupported version of Merlin protocol, please update (plugin is %d, ocamlmerlin binary is %d)."
                            % (protocol_version, value['selected']))
                elif value['latest'] != protocol_version:
                    print("Merlin plugin is outdated, consider updating (plugin is %d, latest is %d)."
                            % (protocol_version, value['latest']))
            else:
                print("Unsupported version of Merlin binary, please update (%s)." % answer)

        except OSError as e:
            print("Failed starting ocamlmerlin. Please ensure that ocamlmerlin binary is executable.")
            raise e

    def command(self, cmd):
        if self.mainpipe == None or self.mainpipe.poll() != None:
            self.restart()
        json.dump(cmd, self.mainpipe.stdin)
        self.mainpipe.stdin.flush()
        line = self.mainpipe.stdout.readline()
        result = json.loads(line)

        for notification in result['notifications']:
            print("(merlin) " + notification['section'] + ": " + notification['message'])
        class_ = result['class']
        value = result['value']
        if class_ == "return":
            return value
        elif class_ == "failure":
            raise Failure(value)
        elif class_ == "error":
            raise Error(value)
        elif class_ == "exception":
            raise MerlinException(value)

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

def context(cmd):
    kind = "auto"
    if vim.eval("exists('b:merlin_kind')") == '1':
        kind = vim.eval("b:merlin_kind")
    name = vim.eval("expand('%:p')")
    dot_merlin = []
    if vim.eval("exists('b:merlin_dot_merlins')") == '1':
        dot_merlin = ["dot_merlin", vim.eval("b:merlin_dot_merlins")]
    return { 'context' : dot_merlin + [kind, name], 'query' : cmd }

def command(*cmd):
    return merlin_process().command(context(cmd))

def sync():
    if vim.eval('exists("b:merlin_tick") && b:merlin_tick == b:changedtick') == '0':
        vim.command('let b:merlin_tick = b:changedtick')
        content = "\n".join(vim.current.buffer) + "\n"
        command("tell", "start", "end", content)

def query(*cmd):
    sync()
    return command(*cmd)

def dump(*cmd):
    print(json.dumps(command('dump', *cmd)))

def dump_to_file(path, *cmd):
    f = open(path, 'w')
    j = command('dump', *cmd)
    f.write(json.dumps(j, indent=4, separators=(',', ': ')))

def uniq(seq):
    seen = set()
    seen_add = seen.add
    return [ x for x in seq if not (x in seen or seen_add(x))]

def vim_is_set(name, default=False):
    if not vim.eval('exists("%s")' % name):
        return default
    return not (vim.eval(name) in ["", "0", "false"])

######## BASIC COMMANDS

def command_version():
    try:
        str = merlin_process().command(["version"])
        print(str)
    except MerlinExc as e:
        try_print_error(e)

def display_load_failures(result):
    if 'failures' in result:
        for failure in result['failures']:
            print(failure)
    return result['result']

def command_find_use(*packages):
    result = catch_and_print(lambda: command('find', 'use', packages))
    return display_load_failures(result)

def command_complete_cursor(base,line,col):
    with_doc = vim_is_set('g:merlin_completion_with_doc', default=True)
    cmd = ["complete", "prefix", base, "at", {'line' : line, 'col': col}]
    if with_doc:
        cmd += ["with", "doc"]
    return command(*cmd)

def command_document(path, line, col):
    try:
        cmd = ["document", path]
        if not (line is None or col is None):
            cmd += ["at", {'line': line, 'col': col}]
        print(query(*cmd))
    except MerlinExc as e:
        try_print_error(e)

def differs_from_current_file(path):
    buf_path = vim.eval("expand('%:p')")
    return buf_path != path


def command_locate(path, line, col):
    try:
        choice = vim.eval('g:merlin_locate_preference')
        if line is None or col is None:
            return query("locate", path, choice)
        else:
            pos_or_err = query("locate", path, choice, "at", {'line': line, 'col': col})
        if not isinstance(pos_or_err, dict):
            print(pos_or_err)
        else:
            l = pos_or_err['pos']['line']
            c = pos_or_err['pos']['col']
            split_method = vim.eval('g:merlin_split_method')
            # save the current position in the jump list
            vim.command("normal! m'")
            if "file" in pos_or_err and differs_from_current_file(pos_or_err['file']):
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

def command_jump(target, line, col):
    try:
        pos_or_err = query("jump", target, "at", {'line': line, 'col': col})
        if not isinstance(pos_or_err, dict):
            print(pos_or_err)
        else:
            l = pos_or_err['pos']['line']
            c = pos_or_err['pos']['col']
            # save the current position in the jump list
            vim.command("normal! m'")
            # TODO: move the cursor using vimscript, so we can :keepjumps?
            vim.current.window.cursor = (l, c)
    except MerlinExc as e:
        try_print_error(e)

def command_occurrences(line, col):
    try:
        lst_or_err = query("occurrences", "ident", "at", {'line':line, 'col':col})
        if not isinstance(lst_or_err, list):
            print(lst_or_err)
        else:
            return lst_or_err
    except MerlinExc as e:
        try_print_error(e)

######## VIM FRONTEND

# Spawn a fresh new process
def vim_restart():
    merlin_process().restart()
    path = vim.eval("expand('%:p')")
    setup_merlin(path)

# Reload changed cmi files then retype all definitions
def vim_reload():
    return query("refresh")

# Complete
def vim_complete_cursor(base, suffix, vimvar):
    vim.command("let %s = []" % vimvar)
    line, col = vim.current.window.cursor
    prep = lambda str: re.sub(re_wspaces, " ", str).replace("'", "''")
    try:
        completions = command_complete_cursor(base,line,col)
        nb_entries = len(completions['entries'])
        if completions['context'] and completions['context'][0] == 'application':
            app = completions['context'][1]
            if not base or base == suffix:
                for label in app['labels']:
                    name = label['name']
                    if not name.startswith(suffix): name = name.replace("?","~")
                    if name.startswith(suffix):
                        nb_entries = nb_entries + 1
                        vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
                                (prep(name),prep(label['name'] + ':' + label['type']),'','~'))
                        vim.command("call add(%s, l:tmp)" % vimvar)
            show_argtype = vim.eval("g:merlin_completion_argtype")
            if ((show_argtype == 'always' or (show_argtype == 'several' and nb_entries > 1))
                    and (not suffix or atom_bound.match(suffix[0]))
                    and app['argument_type'] != "'_a"):
                vim.command("let l:tmp = {'word':'%s','abbr':'<type>','kind':':','menu':'%s','empty':1}" %
                        (prep(suffix),prep(app['argument_type'])))
                vim.command("call insert(%s, l:tmp)" % vimvar)
        for prop in completions['entries']:
            vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
                    (prep(prop['name']),prep(prop['desc']),prep(prop['info']),prep(prop['kind'][:1])))
            vim.command("call add(%s, l:tmp)" % vimvar)
        return (nb_entries > 0)
    except MerlinExc as e:
        try_print_error(e)
        return False

def vim_expand_prefix(base, vimvar):
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
    errors = query("errors")
    bufnr = vim.current.buffer.number
    nr = 0
    for error in errors:
        if error['type'] == 'warning' and vim.eval(ignore_warnings) == 'true':
            continue
        ty = 'w'
        msg = re.sub(re_wspaces, " ", error['message']).replace("'", "''")
        if msg.startswith("Warning "):
            msg = msg[8:]
        elif msg.startswith("Error: "):
            ty = 'e'
            msg = msg[7:]
        lnum = 1
        lcol = 1
        if 'start' in error:
            lnum = error['start']['line']
            lcol = error['start']['col'] + 1
        vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
                (bufnr, lnum, lcol, nr, msg, ty))
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
    command_locate(path, line, col)

def vim_locate_under_cursor():
    vim_locate_at_cursor(None)

# Jump
def vim_jump_to(target):
    line, col = vim.current.window.cursor
    command_jump(target, line, col)

def vim_jump_default():
  vim_jump_to("fun let module match")

# Document
def vim_document_at_cursor(path):
    line, col = vim.current.window.cursor
    command_document(path, line, col)

def vim_document_under_cursor():
    vim_document_at_cursor(None)

# Occurrences
def vim_occurrences(vimvar):
    vim.command("let %s = []" % vimvar)
    line, col = vim.current.window.cursor
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
            start_col = c1
        elif result == "":
            result = current
        else:
            result = result + "\\|" + current
    return "[%s, '%s', '%s']" % (start_col, over, result)

def vim_occurrences_replace(content):
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
    pos = {'line': to_line, 'col': to_col}
    cmd = ["type", "expression", expr, "at", pos]
    try:
        ty = query(*cmd)
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
    (encode,decode) = vim_codec()

    start_line = start['line'] - 1
    b = vim.current.buffer

    fst_line = b[start_line]
    lst_line = b[end['line'] - 1]

    prefix = fst_line[0:start['col']]
    suffix = lst_line[end['col']:len(lst_line)]

    del b[start_line:end['line']]

    txt = decode(prefix) + txt + decode(suffix)
    lines = txt.split('\n')
    lines.reverse()
    nb_lines = 0
    for line in lines:
        nb_lines += 1
        b[start_line:start_line] = [ encode(line) ]

    # Properly reindent the modified lines
    vim.current.window.cursor = (start['line'], 0)
    vim.command("call feedkeys('%d==', 'n')" % nb_lines)

def vim_case_analysis():
    global enclosing_types
    global current_enclosing

    if enclosing_types == []:
        to_line, to_col = vim.current.window.cursor
        pos = {'line':to_line, 'col':to_col}
        try:
            enclosing_types = query("type", "enclosing", "at", pos)
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
        result = query("case", "analysis", "from", tmp['start'], "to", tmp['end'])
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
    to_line, to_col = vim.current.window.cursor
    pos = {'line':to_line, 'col':to_col}
    # deprecated, leave merlin compute the correct identifier
    # atom, a_start, a_end = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
    # offset = to_col - a_start
    # arg = {'expr':atom, 'offset':offset}
    # enclosing_types = command("type", "enclosing", arg, pos)
    try:
        enclosing_types = query("type", "enclosing", "at", pos)
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
    if isinstance(ext, list):
        name = list(map(lambda ext: name + "." + ext, ext))
    elif ext:
        name = name + "." + ext
    return command('which','path',name)

def vim_which_ext(ext,vimvar):
    files = command('which', 'with_ext', ext)
    vim.command("let %s = []" % vimvar)
    for f in sorted(set(files)):
        vim.command("call add(%s, '%s')" % (vimvar, f))

# Extension management
def vim_ext(enable, exts):
    state = enable and 'enable' or 'disable'
    result = catch_and_print(lambda: command('extension', state, exts))
    return display_load_failures(result)

def vim_ext_list(vimvar,enabled=None):
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

def vim_set_flags(*flags):
    result = catch_and_print(lambda: command('flags', 'set', flags))
    return display_load_failures(result)

def vim_get_flags(var):
    result = catch_and_print(lambda: command('flags', 'get'))
    result = " ".join(result)
    vim.command('let %s = "%s"' % (var, result.replace('"','\\"')))

# Stuff

def setup_merlin():
    failures = command("project","get")
    vim.command('let b:dotmerlin=[]')
    # Tell merlin the content of the buffer.
    # This allows merlin idle-job to preload content if nothing else is requested.
    if failures != None:
        fnames = display_load_failures(failures)
        if isinstance(fnames, list):
            vim.command('let b:dotmerlin=[%s]' % ','.join(map(lambda fname: '"'+fname+'"', fnames)))
