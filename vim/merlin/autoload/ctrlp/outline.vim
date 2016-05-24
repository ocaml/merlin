" merlin extension to CtrlP <https://github.com/ctrlpvim/ctrlp.vim>

" Init {{{1
if exists('g:loaded_ctrlp_outline') && g:loaded_ctrlp_outline
  fini
en
let g:loaded_ctrlp_outline = 1

MerlinPy <<EOF

import vim
import merlin

merlin_ctrlp_outlines = []
merlin_ctrlp_send_cmd = lambda cmd: None

def merlin_ctrlp_linearize(prefix, lst):
    for x in lst:
        name = "%s%s" % (prefix, x['name'])
        merlin_ctrlp_outlines.append(
          {'name': name, 'pos': x['start'], 'kind': x['kind']})
        merlin_ctrlp_linearize(name + ".", x['children'])

def merlin_ctrlp_get_outlines():
    merlin_ctrlp_outlines[:] = []
    merlin_ctrlp_linearize("", merlin_ctrlp_send_cmd("outline"))
    merlin_ctrlp_outlines.sort(key = lambda x: len(x['name']))

def merlin_ctrlp_outline_init():
    merlin_ctrlp_get_outlines()
    if len(merlin_ctrlp_outlines) == 0:
        return
    longest = len(merlin_ctrlp_outlines[-1]['name'])
    i = 0
    for x in merlin_ctrlp_outlines:
        name = x['name'].replace("'", "''")
        vim.command("call add(l:modules, '%4d : %*s\t--\t%s')" %
                    (i, longest, name, x['kind']))
        i += 1

def merlin_ctrlp_outline_accept():
    idx = int(vim.eval("a:str").strip().split(' ')[0])
    try:
        x = merlin_ctrlp_outlines[idx]
        l = x['pos']['line']
        c = x['pos']['col']
        vim.current.window.cursor = (l, c)
    except KeyError as e:
        print(str(e))

def merlin_ctrlp_update_and_send(process, ctxt, cmd):
    ctxt['query'] = cmd
    return process.command(ctxt)

def merlin_ctrlp_preinit():
    global merlin_ctrlp_send_cmd
    merlin.sync()
    process = merlin.merlin_process()
    context = merlin.context("fake_query")
    merlin_ctrlp_send_cmd = lambda *cmd: merlin_ctrlp_update_and_send(process, context, cmd)

EOF

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

fu! ctrlp#outline#preinit()
  MerlinPy merlin_ctrlp_preinit()
endf

" Public {{{1
fu! ctrlp#outline#init()
  let l:modules = []
  MerlinPy merlin_ctrlp_outline_init() 
  return l:modules
endf

fu! ctrlp#outline#accept(mode, str)
  call ctrlp#exit()
  MerlinPy merlin_ctrlp_outline_accept()
  silent! normal! zvzz
endf

fu! ctrlp#outline#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
