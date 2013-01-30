" Either source merlin.vim or make it accessible from autoload
" (ensure that merlin.py is in the same directory)
" so merlin.vim

" In the following lines, you can replace *omlet* with *ocaml*
" or whatever FileType you use to edit ml files.

" Activate merlin for given filetype
au FileType omlet call merlin#Register()

" Load merlin project 
" A merlin project is a file named ".merlin" in the same directory of the
" file being edited or in a parent directory (max depth of 3).
au FileType omlet call merlin#LoadProject()

" Enable Syntastic support
" Note that Syntastic may come with a default mode for ocaml.
" You may have to tweak your configuration accordingly.
function! SyntaxCheckers_omlet_GetLocList()
  return merlin#SyntasticGetLocList()
endfunction

" If the mode didn't work for you, ensure that ocamlmerlin binary can be found
" in PATH. 
" Most commands are accessible only from omlet/ocaml buffers, check from an
" appropriate buffer if merlin.vim is effectively loaded.
