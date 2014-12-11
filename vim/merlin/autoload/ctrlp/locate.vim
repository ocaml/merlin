" merlin extension to CtrlP <https://github.com/ctrlpvim/ctrlp.vim>

" Init {{{1
if exists('g:loaded_ctrlp_locate') && g:loaded_ctrlp_locate
	fini
en
let g:loaded_ctrlp_locate = 1

let s:current_dir=expand("<sfile>:p:h")
py if not vim.eval("s:current_dir") in sys.path:
\    sys.path.append(vim.eval("s:current_dir"))

py import locate

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

" Public {{{1
function! ctrlp#locate#update_cursor_pos()
  py locate.update_cursor_pos()
endfunction

function! ctrlp#locate#init()
  return []
endfunction

function! ctrlp#locate#filter(items, str, limit, mmode, ispath, crfile, regex)
  let l:compl = []
  py locate.do_expand(vim.eval("a:str"), "l:compl")
  return l:compl
endfunction

fu! ctrlp#locate#accept(mode, str)
  call ctrlp#exit()
  py merlin.vim_locate_at_cursor(vim.eval("a:str"))
  silent! normal! zvzz
endf

fu! ctrlp#locate#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
