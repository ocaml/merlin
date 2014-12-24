" Vim syntax file for editing merlin project files
if exists("b:current_syntax")
  finish
endif

syn keyword merlinKeyword S B PKG REC EXT PRJ FLG
syn match merlinComment "\v#.*$"

hi link merlinKeyword Keyword
hi link merlinComment Comment

let b:current_syntax = "merlin"

