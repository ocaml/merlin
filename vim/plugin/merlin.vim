" Either source merlin.vim or make it accessible from autoload
" (ensure that merlin.py is in the same directory)
" so merlin.vim

" In the following lines, you can replace *omlet* with *ocaml*
" or whatever FileType you use to edit ml files.

" Activate merlin for given filetype
au FileType omlet call merlin#Register()
au FileType ocaml call merlin#Register()

" Load merlin project 
" A merlin project is a file named ".merlin" in the same directory of the
" file being edited or in a parent directory (max depth of 3).
au FileType omlet call merlin#LoadProject()
au FileType ocaml call merlin#LoadProject()

" Flush buffer and dependencies after :make
" Note: reloading Core can take some time
au QuickFixCmdPost * call merlin#Reload()

" If the mode didn't work for you, ensure that ocamlmerlin binary can be found
" in PATH. 
" Most commands are accessible only from omlet/ocaml buffers, check from an
" appropriate buffer if merlin.vim is effectively loaded.

" If you are using syntastic and don't want warnings notified, set the following
" variable to "true"
let g:merlin_ignore_warnings = "false"

" Highlight the expression which type is given
hi EnclosingExpr ctermbg=17 guibg=LightGreen
