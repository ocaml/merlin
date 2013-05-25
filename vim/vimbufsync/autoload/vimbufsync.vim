" VIMBUFSYNC, original version by Frederic Bour <frederic.bour _ lakaban.net>
" Released under the terms of the WTFPL license. See below.


"            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"                    Version 2, December 2004
" 
" Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
" 
" Everyone is permitted to copy and distribute verbatim or modified
" copies of this license document, and changing it is allowed as long
" as the name is changed.
" 
"            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
" 
"  0. You just DO WHAT THE FUCK YOU WANT TO.
"

let s:current_dir=expand("<sfile>:p:h")
py import sys, vim
py if not vim.eval("s:current_dir") in sys.path:
\    sys.path.append(vim.eval("s:current_dir"))

function! vimbufsync#init()
endfunction
