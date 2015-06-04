" merlin extension to CtrlP <https://github.com/ctrlpvim/ctrlp.vim>

" Init {{{1
if exists('g:loaded_ctrlp_outline') && g:loaded_ctrlp_outline
	fini
en
let g:loaded_ctrlp_outline = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#outline#init()',
	\ 'accept': 'ctrlp#outline#accept',
	\ 'lname': 'outline',
	\ 'sname': 'ml',
	\ 'type': 'tabs',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

python << EOF
outlines = []

def linearize(prefix, lst):
  for x in lst:
    name = "%s%s" % (prefix, x['name'])
    outlines.append({'name': name, 'pos': x['start'], 'kind': x['kind']})
    linearize(name + ".", x['children'])

def get_outlines():
  global outlines
  outlines = []
  result = merlin.command("outline")
  linearize("", result)
  outlines.sort(key = lambda x: len(x['name']))
EOF

" Public {{{1
fu! ctrlp#outline#init()
  let l:modules = []
  python << EOF
get_outlines()
longest = len(outlines[-1]['name'])
i = 0
for x in outlines:
  name = x['name'].replace("'", "''")
  vim.command("call add(l:modules, '%4d : %*s\t--\t%s')" % (i, longest, name, x['kind']))
  i += 1
EOF
  return l:modules
endf

fu! ctrlp#outline#accept(mode, str)
  call ctrlp#exit()
  python << EOF
idx = int(vim.eval("a:str").strip().split(' ')[0])

try:
  x = outlines[idx]
  l = x['start']['line']
  c = x['start']['col']
  vim.current.window.cursor = (l, c)
except KeyError, e:
  print(str(e))
EOF
silent! normal! zvzz
endf

fu! ctrlp#outline#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
