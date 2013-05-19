" Activate merlin on current buffer
call merlin#Register()

" Load merlin project 
" A merlin project is a file named ".merlin" in the same directory of the
" file being edited or in a parent directory (max depth of 3).
call merlin#LoadProject()
