" Vim syntax file for editing merlin project files
if exists("b:current_syntax")
  finish
endif

syn keyword merlinKeyword S B PKG
syn match merlinComment "\v#.*$"

let b:current_syntax = "merlin"

hi link merlinKeyword Keyword
" TODO
hi link merlinPath String
hi link merlinFindlib Function
hi link merlinComment Comment

