# VIMBUFSYNC, original version by Frederic Bour <frederic.bour _ lakaban.net>
# Released under the terms of the WTFPL license. See below.

#
#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#                    Version 2, December 2004
#
# Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
#
# Everyone is permitted to copy and distribute verbatim or modified
# copies of this license document, and changing it is allowed as long
# as the name is changed.
#
#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
#
#  0. You just DO WHAT THE FUCK YOU WANT TO.
#

import vim
import re
import os
import sys
import bisect
from itertools import groupby, izip

version = [0,1,0] # 0.1.0

def check_version(v,who="a plugin"):
  """Call it with required version number to print error message if needed"""
  global version
  if isinstance(v,str): v = map(int,v.split("."))
  if v > version:
    msg = "vimbufsync: current version is %s but %s requires version %s (installed in \"%s\"). Please update."
    sys.stderr.write(msg % (".".join(map(str,version)), who, ".".join(map(str,v)), os.path.abspath(__file__)))

changes_pattern = re.compile('(>)?\s*(\d+)\s*(\d+)\s*(\d+)\s*(.*)$')

def changes_of_buffer(nr):
  """extract result of "changes" command on a specific buffer
     or None for deleted buffers"""
  cmd = "let selnr = %d\n%s" % (nr,
"let curnr = bufnr('%')\n\
redir => changes_string\n\
if curnr == selnr\n\
  silent changes\n\
else\n\
  noa | execute 'buffer ' . selnr | silent changes | execute 'buffer ' . curnr\n\
endif\n\
redir END")
  try:
    vim.command(cmd)
    return vim.eval("changes_string").split("\n")
  except vim.error:
    return None

def extract_change(match):
  return (int(match.group(3)),int(match.group(4)),match.group(5))

def extract_changes(nr):
  """Returns structured representation of buffer changes,
     or None if buffer deleted or not modified"""
  global changes_pattern
  # compute changes as a list of match objects
  changes = changes_of_buffer(nr)
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
  return dict((k, len(list(v))) for k,v in groupby(sorted(map(extract_change,changes))))

class BufferRevision:
  """A BufferRevision tracks changed lines starting from a particular
     version of the buffer contents."""
  def __init__(self, buf, rev):
    self._buf = buf
    self._rev = rev
    self._last_pos = (len(buf)+1,0)
    self._last_rev = rev

  def buf(self):
    """Access to the ShadowBuffer object, or None if the buffer has
       been deleted"""
    return sync_buffer(self._buf)

  def bufnr(self):
    """Number of the buffer (unique accross a complete vim session)"""
    return self._buf.number

  def pos(self):
    """Position of the first character that differs between revision and
       current contents. This can only decrease over time, when (1,1) is
       reached, the buffer is completely different"""
    buf = self.buf()
    if buf and self._last_rev != buf._revision() and self._last_pos > (1,1):
      (self._last_pos,self._last_rev) = buf._validate_revision(self._rev)
    return self._last_pos

  def line(self):
    """Index of the first line that differs between revision and current
       contents. This can only decrease over time, when 1 is reached, the
       buffer is completely different"""
    return self.pos()[0]

  def col(self):
    """Index of the first column that differs between revision and current
       contents in first changed line."""
    return self.pos()[1]

class ShadowBuffer:
  """Maintains state and provides heuristics needed to quickly find what
     changed in a buffer"""
  def __init__(self,buf):
    self._buf = buf
    self._rev = 0
    self.clear()

  def clear(self):
    self._rev += 1
    self._shadow = []
    self._revisions_pos = []
    self._revisions_num  = []
    self._revobj = None
    self._changes = None

  def bufnr(self):
    """Buffer number"""
    return self._buf.number

  def _find_changes(self):
    previous = self._changes
    changes = extract_changes(self._buf.number)
    self._changes = changes

    first_pass = previous == None
    if first_pass:
      return None
    return [k for (k,v) in changes.items()
              if not k in previous or previous[k] < v]

  def _find_changed_line(self):
    changes = self._find_changes()
    if changes == None:
      return 1
    lines = set(lin for lin,col,txt in changes)
    if lines:
      return min(lines)
    return None

  def _revision(self):
    return self._rev

  def revision(self):
    """Returns last synced revision as a BufferRevision object, buffer
       contents may have changed since then"""
    if not (self._revobj and self._revobj._rev == self._rev):
      self._revobj = BufferRevision(self._buf, self._rev)
    return self._revobj

  def _invalidate_lines(self,line,col):
    self._rev += 1

    last = bisect.bisect_left(self._revisions_pos, (line,col))
    if last:
      self._revisions_pos = self._revisions_pos[:last]
      self._revisions_num = self._revisions_num[:last]
    else:
      self._revisions_pos = []
      self._revisions_num = []
    self._revisions_pos.append((line,col))
    self._revisions_num.append(self._rev)

  def _validate_revision(self,rev):
    index = bisect.bisect_right(self._revisions_num, rev)
    if index:
      return (self._revisions_pos[index],self._rev)
    else:
      return ((1,1),self._rev)

  def sync(self):
    """Synchronize with buffer if needed and return a BufferRevision object
       guaranteed to match current contents"""
    line = self._find_changed_line()
    if line:
      line_max = min(len(self._shadow),len(self._buf))
      line = min(line,line_max)
      # heuristic: find 3 equal non-blank lines in a row
      in_a_row = 0
      line_count = 0
      while line > 0 and in_a_row < 3:
        line -= 1
        if self._shadow[line] == self._buf[line]:
          line_count += 1
          if self._shadow[line] != "":
            in_a_row += 1
        else:
          in_a_row = 0
          line_count = 0
      line += line_count
      # increment line while unchanged
      while line < line_max and self._shadow[line] == self._buf[line]:
        line += 1
      # find changed column
      if line < line_max:
        s1 = self._shadow[line]
        s2 = self._buf[line]
        col = [i for i,(a1,a2)  in enumerate(izip(s1,s2)) if a1!=a2]
        if col: col = col[0]+1
        else:   col = min(len(s1),len(s2))+1
      else:
        col = 1
      # Switch back to 1-indexed array
      line += 1
      # update shadow buffer
      if line < 1:
        self._shadow[:] = self._buf[:]
      else:
        self._shadow[line-1:] = self._buf[line-1:]
      # new revision
      self._invalidate_lines(line,col)
    return self.revision()

shadow_buffers = dict()
deletion_listeners = set()

class DeletionListener:
  """Subclass to stay informed of deleted buffers"""
  def deleted(self,shadow,nr):
    pass

  def register(self):
    global deletion_listeners
    deletion_listeners.add(self)

  def unregister(self):
    global deletion_listeners
    deletion_listeners.remove(self)

class LambdaDeletionListener(DeletionListener):
  def __init__(self,fun):
    self.fun = fun

  def deleted(self,buf,nr):
    self.fun(buf,nr)

def garbage_collect():
  """Garbage collect deleted buffers"""
  global shadow_buffers, deletion_listeners
  for (nr,shadow) in shadow_buffers.items():
    if not (shadow._buf in vim.buffers):
      del shadow_buffers[nr]
      for l in deletion_listeners:
        try:
          l.deleted(shadow,nr)
        except:
          pass

def sync_buffer(vimbuf):
  """Access ShadowBuffer associated with a vim-buffer object. Don't use directly,
     prefers sync()"""
  global shadow_buffers
  if not vimbuf in vim.buffers:
    garbage_collect()
    return None
  nr = vimbuf.number
  if not (nr in shadow_buffers):
    garbage_collect()
    shadow_buffers[nr] = ShadowBuffer(vimbuf)
  return shadow_buffers[nr]

def find_buffer_with_number(nr):
  for b in vim.buffers:
    if b.number == nr: return b
  return None

def sync(vimbuf=None):
  """Returns a BufferRevision synchronized with the buffer numbered nr,
     current buffer if none specified, or None if buffer got deleted"""
  if not vimbuf:
    vimbuf = vim.current.buffer
  elif type(vimbuf) == int:
    vimbuf = find_buffer_with_number(vimbuf)
  if not vimbuf: return None
  buf = sync_buffer(vimbuf)
  if buf: return buf.sync()
  return None
