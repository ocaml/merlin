import vim
import os
import merlin

line = 0
col = 0

def update_cursor_pos():
    global line, col
    line, col = vim.current.window.cursor

def do_expand(base, vimvar):
  print("test")
  try:
    l = merlin.command("expand", "prefix", base, "at", {'line' : line, 'col': col})
    l = map(lambda prop: prop['name'], l)
    l = merlin.uniq(sorted(l))
    for prop in l:
      name = prop.replace("'", "''")
      vim.command("call add(%s, '%s')" % (vimvar, name))
  except merlin.MerlinExc as e:
    merlin.try_print_error(e)
