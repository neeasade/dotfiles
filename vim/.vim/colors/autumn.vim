" Author: Mvdw
" Email: <mvdw at airmail dot cc>
" A terminal based vim colorscheme.
" It is designed to be ONLY for terminal use. It requires you to use the 16 ANSI
" defined colors.


" {{{1 Color reference list.
" Background:       #170F0D
" Foreground:       #746C48
" Cursor:           #6BA16C
" ANSI 0:           #392925
" ANSI 1:           #98724C
" ANSI 2:           #908F32
" ANSI 3:           #AA964C
" ANSI 4:           #7B854E
" ANSI 5:           #6B5644
" ANSI 6:           #5C5142
" ANSI 7:           #C8B55B
" ANSI 8:           #544B2E
" ANSI 9:           #AF652F
" ANSI 10:          #C3C13D
" ANSI 11:          #C8B55B
" ANSI 12:          #70A16C
" ANSI 13:          #98724C
" ANSI 14:          #778725
" ANSI 15:          #E4DC8C

hi clear
if exists("syntax_on")
    syntax reset
endif
set background=dark

if &fdm != 'marker'
    set fdm=marker
endif

" {{{1 These are the default highlighting groups.  These groups are used by the
" 'highlight' option default.  Note that the highlighting depends on the value
" of 'background'.  You can see the current settings with the ":highlight"
" command.
"
" ColorColumn   Used for the columns set with 'colorcolumn'.
" Directory     Directory names (and other special names in listings).
" ErrorMsg      Error messages on the command line.
" VertSplit     The column separating vertically split windows.
" Folded        Line used for closed folds.
" SignColumn    Column where signs are displayed.
" IncSearch     'incsearch' highlighting; also used for the text replaced with
"               ":s///c"
" LineNr        Line number for ":number" and ":#" commands, and when 'number'
"               or 'relativenumber' option is set.
" MatchParen    The character under the cursor or just before it, if it
"               is a paired bracket, and its match.
" ModeMsg       'showmode' message (e.g., "-- INSERT --").
" MoreMsg       More-prompt.
" NonText       '~' and '@' at the end of the window, characters from
"               'showbreak' and other characters that do not really exist in
"               the text (e.g., ">" displayed when a double-wide character
"               doesn't fit at the end of the line).
" Normal        Normal text.
" Pmenu         Popup menu: normal item.
" PmenuSel      Popup menu: selected item.
" PmenuSbar     Popup menu: scrollbar.
" PmenuThumb    Popup menu: Thumb of the scrollbar.
" Question      Hit-enter prompt and yes/no questions.
" Search        Last search pattern highlighting (see 'hlsearch').
"               Also used for highlighting the current line in the quickfix
"               window and similar items that need to stand out.
" SpecialKey    Meta and special keys listed with ":map", also for text used
"               to show unprintable characters in the text, 'listchars'.
"               Generally: text that is displayed differently from what it
"               really is.
" SpellBad      Word that is not recognized by the spellchecker. spell
"               This will be combined with the highlighting used otherwise.
" SpellCap      Word that should start with a capital. spell
"               This will be combined with the highlighting used otherwise.
" StatusLine    Status line of current window.
" StatusLineNC  Status lines of not-current windows.
"               Note: if this is equal to "StatusLine" Vim will use "^^^" in
"               The status line of the current window.
" Title         Titles for output from ":set all", ":autocmd" etc.
" Visual        Visual mode selection.
" VisualNOS     Visual mode selection when vim is "Not Owning the Selection".
"               Only X11 Gui's gui-x11 and xterm-clipboard supports this.
" WarningMsg    Warning messages.
" WildMenu      Current match in 'wildmenu' completion. }}}

hi Normal       ctermfg=none ctermbg=none cterm=none
hi ColorColumn  ctermfg=none ctermbg=0    cterm=none
hi SpecialKey 	ctermfg=10   ctermbg=none cterm=none
hi NonText 		ctermfg=4    ctermbg=none cterm=bold
hi Directory 	ctermfg=12   ctermbg=none cterm=none
hi ErrorMsg 	ctermfg=7    ctermbg=none cterm=bold
hi IncSearch 	ctermfg=14   ctermbg=none cterm=reverse
hi SpellBad     ctermfg=none ctermbg=none cterm=none
hi SpellCap     ctermfg=none ctermbg=none cterm=none
hi Search 		ctermfg=7    ctermbg=none cterm=bold,italic
hi MoreMsg 		ctermfg=2    ctermbg=none cterm=none
hi ModeMsg 		ctermfg=14   ctermbg=none cterm=bold
hi LineNr 		ctermfg=5    ctermbg=none cterm=none
hi Question 	ctermfg=2    ctermbg=none cterm=none
hi StatusLine 	ctermfg=8    ctermbg=none cterm=bold
hi StatusLineNC ctermfg=6    ctermbg=none cterm=bold
hi VertSplit 	ctermfg=0    ctermbg=0    cterm=bold
hi Title 		ctermfg=12   ctermbg=none cterm=bold
hi Visual 		ctermfg=none ctermbg=0    cterm=italic
hi VisualNOS 	ctermfg=none ctermbg=none cterm=bold,underline
hi WarningMsg 	ctermfg=10   ctermbg=none cterm=none
hi WildMenu 	ctermfg=14   ctermbg=none cterm=none
hi Folded 		ctermfg=2    ctermbg=none cterm=bold
hi MatchParen   ctermfg=none ctermbg=none cterm=italic,bold
hi SignColumn   ctermfg=10   ctermbg=none cterm=bold

" {{{1 To be able to allow each user to pick his favorite set of colors, there must
" be preferred names for highlight groups that are common for many languages.
" These are the suggested group names (if syntax highlighting works properly
" you can see the actual color, except for "Ignore"):
"
" 	*Comment	any comment
"
" 	*Constant	any constant
" 	 String		a string constant: "this is a string"
" 	 Character	a character constant: 'c', '\n'
" 	 Number		a number constant: 234, 0xff
" 	 Boolean	a boolean constant: TRUE, false
" 	 Float		a floating point constant: 2.3e10
"
" 	*Identifier	any variable name
" 	 Function	function name (also: methods for classes)
"
" 	*Statement	any statement
" 	 Conditional	if, then, else, endif, switch, etc.
" 	 Repeat		for, do, while, etc.
" 	 Label		case, default, etc.
" 	 Operator	"sizeof", "+", "*", etc.
" 	 Keyword	any other keyword
" 	 Exception	try, catch, throw
"
" 	*PreProc	generic Preprocessor
" 	 Include	preprocessor #include
" 	 Define		preprocessor #define
" 	 Macro		same as Define
" 	 PreCondit	preprocessor #if, #else, #endif, etc.
"
" 	*Type		int, long, char, etc.
" 	 StorageClass	static, register, volatile, etc.
" 	 Structure	struct, union, enum, etc.
" 	 Typedef	A typedef
"
" 	*Special	any special symbol
" 	 SpecialChar	special character in a constant
" 	 Tag		you can use CTRL-] on this
" 	 Delimiter	character that needs attention
" 	 SpecialComment	special things inside a comment
" 	 Debug		debugging statements
"
" 	*Underlined	text that stands out, HTML links
"
" 	*Ignore		left blank, hidden  |hl-Ignore|
"
" 	*Error		any erroneous construct
"
" 	*Todo		anything that needs extra attention; mostly the
" 			keywords TODO FIXME and XXX
"
" The names marked with * are the preferred groups; the others are minor groups.
" For the preferred groups, the "syntax.vim" file contains default highlighting.
" The minor groups are linked to the preferred groups, so they get the same
" highlighting.  You can override these defaults by using ":highlight" commands
" after sourcing the "syntax.vim" file.
" The default highlighting groups.
" More documentation about these highlighting groups can be found by doing:
" :help syntax. }}}

hi Comment      ctermfg=7    ctermbg=none cterm=italic
hi Constant     ctermfg=1    ctermbg=none cterm=none
hi String       ctermfg=10   ctermbg=none cterm=italic
hi Character    ctermfg=10   ctermbg=none cterm=bold
hi Number       ctermfg=10   ctermbg=none cterm=none
hi Boolean      ctermfg=6    ctermbg=none cterm=bold
hi Float        ctermfg=10   ctermbg=none cterm=none
hi Special      ctermfg=5    ctermbg=none cterm=none
hi Identifier   ctermfg=12   ctermbg=none cterm=bold
hi Function     ctermfg=8    ctermbg=none cterm=bold
hi Statement    ctermfg=3    ctermbg=none cterm=none
hi Conditional  ctermfg=12   ctermbg=none cterm=none
hi Repeat       ctermfg=12   ctermbg=none cterm=none
hi Label        ctermfg=11   ctermbg=none cterm=bold
hi Operator     ctermfg=2    ctermbg=none cterm=none
hi Keyword      ctermfg=7    ctermbg=none cterm=none
hi Exception    ctermfg=4    ctermbg=none cterm=none
hi PreProc      ctermfg=5    ctermbg=none cterm=none
hi Special      ctermfg=14   ctermbg=none cterm=none
hi SpecialChar  ctermfg=14   ctermbg=none cterm=none
hi Tag          ctermfg=11   ctermbg=none cterm=bold
hi Debug        ctermfg=0    ctermbg=none cterm=bold,italic
hi Type         ctermfg=2    ctermbg=none cterm=none
hi Ignore       ctermfg=7    ctermbg=none cterm=bold
hi Error        ctermfg=7    ctermbg=0    cterm=bold
hi Todo         ctermfg=12   ctermbg=none cterm=none

" https://github.com/scrooloose/syntastic/
hi SyntasticErrorLine   ctermfg=none ctermbg=0    cterm=none
hi SyntasticErrorSign   ctermfg=9    ctermbg=0    cterm=none
hi SyntasticWarningLine ctermfg=none ctermbg=0    cterm=none
hi SyntasticWarningSign ctermfg=9    ctermbg=0    cterm=none

" https://github.com/ap/vim-buftabline/
hi BufTabLineCurrent    ctermfg=5 ctermbg=none cterm=none
hi BufTabLineActive     ctermfg=5 ctermbg=none cterm=none
hi BufTabLineHidden     ctermfg=2 ctermbg=none cterm=none
hi BufTabLineFill       ctermfg=2 ctermbg=none cterm=none
