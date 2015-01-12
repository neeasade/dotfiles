" Vim color scheme
"
" Name:         blackboard.vim
" Maintainer:   Ben Wyrosdick <ben.wyrosdick@gmail.com> 
" Last Change:  2 July 2008
" License:      public domain
" Version:      1.1

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let g:colors_name = "blackboard"

"GUI Colors
highlight Normal guifg=White   guibg=#0B1022 ctermbg=233
highlight Cursor guifg=Black   guibg=Yellow
highlight CursorLine guibg=#191E2F ctermbg=235
highlight LineNr guibg=#323232 ctermbg=236 guifg=#888888 ctermfg=102
highlight Folded guifg=White
highlight Visual guibg=#283A76 ctermbg=24

"General Colors
highlight Comment guifg=#AEAEAE ctermfg=145
highlight Constant guifg=#D8FA3C ctermfg=191
highlight Keyword guifg=#FFDE00 ctermfg=220
highlight String guifg=#61CE3C ctermfg=77
highlight Type guifg=#84A7C1 ctermfg=109
highlight Identifier guifg=#61CE3C ctermfg=77 gui=NONE
highlight Function guifg=#FF5600 ctermfg=202 gui=NONE
highlight clear Search
highlight Search guibg=#1C3B79 ctermbg=24
highlight PreProc guifg=Grey

"Ruby Colors
highlight link rubyClass Keyword
highlight link rubyDefine Keyword
highlight link rubyConstant Type
highlight link rubySymbol Constant
highlight link rubyStringDelimiter rubyString
highlight link rubyInclude Keyword
highlight link rubyAttribute Keyword
highlight link rubyInstanceVariable Normal

"Rails Colors
highlight link railsMethod Type

"Invisible character colors
highlight NonText guifg=#4a4a59 ctermfg=239
highlight SpecialKey guifg=#4a4a59 ctermfg=239

"HTML tags
hi Title                     guifg=#FFFFFF ctermfg=15

hi link htmlTag              xmlTag
hi link htmlTagName          xmlTagName
hi link htmlEndTag           xmlEndTag

hi xmlTag                    guifg=#8FA5CE ctermfg=110
hi xmlTagName                guifg=#8FA5CE ctermfg=110
hi xmlEndTag                 guifg=#8FA5CE ctermfg=110
