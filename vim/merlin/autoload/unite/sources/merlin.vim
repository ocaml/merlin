let s:source_ml = {
      \ "name" : "merlin/ml",
      \ "description" : "Navigate to OCaml module",
      \ "default_kind": "command",
      \}

let s:source_mli = {
      \ "name" : "merlin/mli",
      \ "description" : "Navigate to OCaml module signature",
      \ "default_kind": "command",
      \}

let s:source_findlib = {
      \ "name" : "merlin/use",
      \ "description" : "Use findlib module",
      \ "default_kind": "command",
      \}

function! s:source_findlib.gather_candidates(args, context)
  let l:pkgs = []
  py merlin.vim_findlib_list("l:pkgs")
  return map(l:pkgs, '{
        \ "word": v:val,
        \ "source": "merlin/use",
        \ "action__command": "MerlinUse ".v:val,
        \ }')
endfunction

function! s:source_ml.gather_candidates(args, context)
  let l:modules = []
  py merlin.vim_which_ext([".ml", ".mli"], "l:modules")
  return map(l:modules, '{
        \ "word": v:val,
        \ "source": "merlin/ml",
        \ "action__command": "ML ".v:val,
        \ }')
endfunction

function! s:source_mli.gather_candidates(args, context)
  let l:modules = []
  py merlin.vim_which_ext([".mli", ".ml"], "l:modules")
  return map(l:modules, '{
        \ "word": v:val,
        \ "source": "merlin/mli",
        \ "action__command": "MLI ".v:val,
        \ }')
endfunction

function! unite#sources#merlin#define()
  return [s:source_ml, s:source_mli, s:source_findlib]
endfunction
