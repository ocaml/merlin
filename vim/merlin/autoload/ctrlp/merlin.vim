" merlin extension to CtrlP <https://github.com/ctrlpvim/ctrlp.vim>

" Init {{{1
if exists('g:loaded_ctrlp_merlin') && g:loaded_ctrlp_merlin
	fini
en
let g:loaded_ctrlp_merlin = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#merlin#init()',
	\ 'accept': 'ctrlp#merlin#accept',
	\ 'lname': 'merlin',
	\ 'sname': 'ml',
	\ 'type': 'tabs',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

python << EOF
outlines = []

def qualify(prefix, node):
  tmp = "%s.%s" % (prefix, node['name'])
  node['name'] = tmp
  return node

def children(node):
  return map(lambda x: qualify(node['name'], x), node['children'])

def get_outlines():
  global outlines
  outlines = []
  tmp1 = merlin.command("outline")
  tmp2 = map(lambda x: [x] if x['kind'] != "Module" else children(x), tmp1)
  map(outlines.extend, tmp2)
EOF

" Public {{{1
fu! ctrlp#merlin#init()
  let l:modules = []
  python << EOF
get_outlines()
longest = reduce(lambda x, y: max(x,len(y['name'])), outlines, 0)
for x in outlines:
  name = x['name'].replace("'", "''")
  # Listing only top bindings because fuck it.
  vim.command("call add(l:modules, '%*s\t--\t%s')" % (longest, name, x['kind']))
EOF
  return l:modules
endf

fu! ctrlp#merlin#accept(mode, str)
  call ctrlp#exit()
  python << EOF
matching_name = vim.eval("a:str").strip().split('\t')[0]

for x in outlines:
  if x['name'] == matching_name:
    l = x['pos']['line']
    c = x['pos']['col']
    vim.current.window.cursor = (l, c)
EOF
silent! normal! zvzz
endf

fu! ctrlp#merlin#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
