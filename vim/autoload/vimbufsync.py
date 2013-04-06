import vim
import re
import os
import sys
import bisect
from itertools import groupby

changes_pattern = re.compile('(>)?\s*(\d+)\s*(\d+)\s*(\d+)\s*(.*)$')

def changes_of_buffer(nr):
  # extract result of "changes" command on specified buffer
  cmd = "let selnr = %d\n%s" % (nr,
"let curnr = bufnr('%')\n\
redir => changes_string\n\
if curnr == selnr\n\
  silent changes\n\
else\n\
  noa | execute 'buffer ' . selnr | silent changes | execute 'buffer ' . curnr\n\
endif\n\
redir END")
  vim.command(cmd)
  return vim.eval("changes_string").split("\n")

def extract_change(match):
  return (int(match.group(3)),int(match.group(4)),match.group(5))

def extract_changes(nr):
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
  def __init__(self, nr, rev):
    self.nr = nr
    self.revision = revision

class ShadowBuffer:
  def __init__(self,nr):
    self.nr = nr
    self.revision = 0
    self.clear()

  def clear(self):
    self.revision += 1
    self.shadow = []
    self.revisions = []
    self.revobj = None
    self.changes = None

  def _find_changes(self):
    first_pass = self.changes == None
    changes = changes_of_buffer(self.nr)
    self.changes = changes
    if first_pass:
      return None
    if len(changes) == 0:
      return []
    return [k for (k,v) in changes.items()
              if not k in previous or previous[k] < v]

  def _find_line(self,changes):
    if changes == None:
      return 0
    lines = set(lin for lin,col,txt in changes) 
    if lines:
      return min(lines)
    return None

  def revision(self):
    if not (self.revobj and self.revobj.revision == self.revision):
      self.revobj = BufferRevision(self.nr, self.revision)
    return self.revobj

  def _invalidate_lines(self,line):
    self.revision += 1

    last = bisect.bisect_left(self.revisions, (line,0))
    if last: self.revisions = self.revisions[:last]
    else:    self.revisions = []
    self.revisions.append(line,self.revision)

  def sync(self):
    try:
      buf = vim.buffers[self.nr]
    except IndexError:
      return None
    changes = self._find_changes()
    line = self._find_line(changes)
    if not line:
      return None
    line = min(line,len(self.shadow),len(buf))

    # heuristic: find 3 equal non-blank lines in a row
    in_a_row = 0
    line_count = 0
    while line > 0 and in_a_row < 3:
      line -= 1
      if self.shadow[line] == buf[line]:
        line_count += 1
        if self.shadow[line] != "":
          in_a_row += 1
      else:
        in_a_row = 0
        line_count = 0
    line += 1 + line_count

    # update shadow buffer
    if line < 1:
      self.shadow[:] = buf[:]
    else:
      self.shadow[line-1:] = buf[line-1:]

    # new revision
    self._invalidate_lines(line)
    return self.revision()

shadow_buffers = dict()

def sync_buffer(nr):
  global shadow_buffers
  try:
    buf = vim.buffers[nr]
    if not (nr in shadow_buffers):
      # Garbage collect deleted buffers
      for nr in shadow_buffers:
        try:
          vim.buffers[nr]
        except IndexError:
          del shadow_buffers[nr]
      shadow_buffers[nr] = ShadowBuffer(nr)
    return shadow_buffers[nr]
  except IndexError:
    if nr in shadow_buffers: del shadow_buffers[nr]
    return None
