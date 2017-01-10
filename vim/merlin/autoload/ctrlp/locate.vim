" merlin extension to CtrlP <https://github.com/ctrlpvim/ctrlp.vim>

" Init {{{1
if exists('g:loaded_ctrlp_locate') && g:loaded_ctrlp_locate
	fini
en
let g:loaded_ctrlp_locate = 1

MerlinPy <<EOF
import vim
import merlin

merlin_ctrlp_locate_line = 0
merlin_ctrlp_locate_col = 0

def merlin_ctrlp_locate_update_cursor_pos():
    global merlin_ctrlp_locate_pos
    merlin_ctrlp_locate_pos = vim.current.window.cursor

def merlin_ctrlp_locate_do_expand(base, vimvar):
    try:
        pos = (merlin_ctrlp_locate_pos[0], merlin_ctrlp_locate_pos[1])
        l = merlin.command("expand-prefix", "-prefix", base, 
                           "-position", merlin.fmtpos(pos))
        l = l['entries']
        l = map(lambda prop: prop['name'], l)
        l = merlin.uniq(sorted(l))
        for prop in l:
          name = prop.replace("'", "''")
          vim.command("call add(%s, '%s')" % (vimvar, name))
    except merlin.MerlinExc as e:
        merlin.try_print_error(e)

EOF

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#locate#init()',
	\ 'accept': 'ctrlp#locate#accept',
	\ 'lname': 'locate',
	\ 'sname': 'ml',
	\ 'type': 'tabs',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)
let s:init_string = "(* Start typing to get a list of identifiers. *)"

" Public {{{1
function! ctrlp#locate#update_cursor_pos()
  MerlinPy merlin_ctrlp_locate_update_cursor_pos()
endfunction


function! ctrlp#locate#init()
  return [s:init_string]
endfunction

function! ctrlp#locate#filter(items, str, limit, mmode, ispath, crfile, regex)
  let l:compl = []
  MerlinPy merlin_ctrlp_locate_do_expand(vim.eval("a:str"), "l:compl")
  return l:compl
endfunction

fu! ctrlp#locate#accept(mode, str)
  call ctrlp#exit()
  if a:str == s:init_string
    " do nothing
  else
    MerlinPy merlin.vim_locate_at_cursor(vim.eval("a:str"))
    silent! normal! zvzz
  endif
endf

fu! ctrlp#locate#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
