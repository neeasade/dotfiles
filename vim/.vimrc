

" Vundle Settings
set nocompatible
filetype off
set rtp+=/home/neeasade/.vim/bundle/vundle
call vundle#begin()
"""""""""""
" Bundles "
"""""""""""
Plugin 'gmarik/Vundle.vim'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-dispatch'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'garbas/vim-snipmate'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'vim-scripts/tlib'
Plugin 'honza/vim-snippets'
Plugin 'thisivan/vim-bufexplorer'
Plugin 'godlygeek/tabular'
Plugin 'gregsexton/MatchTag'
Plugin 'semmons99/vim-ruby-matchit'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kana/vim-textobj-user'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'vim-scripts/Auto-Pairs'
Plugin 'terryma/vim-smooth-scroll'
Plugin 'elzr/vim-json'
Plugin 'scrooloose/syntastic'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'ervandew/supertab'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'Raimondi/delimitMate'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'terryma/vim-multiple-cursors'
"""""""""""""""
" Bundles END "
"""""""""""""""
call vundle#end()
filetype plugin indent on

runtime! bundle/snipmate-snippets/support_functions.vim
runtime macros/matchit.vim

" autocmd BufWritePre * :%s/\s\+$//e

set title
set hidden

" Numbers
set number
set numberwidth=4
set ruler

"exe "set path=".expand("$PATH")

syntax enable
set nomodeline
set backspace=indent,eol,start                          " Backspace will delete EOL chars, as well as indents
set matchpairs+=<:>                                     " For characters that forms pairs for using % commands, this is for HTML Tags
set shortmess=atToOI                                    " To avoid the 'Hit Enter' prompts caused by the file messages
set iskeyword+=_,$,@,%,#                                " Keywords are use to searching and recognized with many commands
set history=1000
set undolevels=1000
set confirm
set updatetime=1500

set mousehide                                           " Hide the mouse cursor when typing
set mouse=a

" Instantly leave insert mode when pressing <ESC>"
" This works by disabling the mapping timeout completely in normal
" mode, and enabling it in insert mode with a very low timeout length.
augroup fastescape
	autocmd!

	set notimeout
	set ttimeout
	set timeoutlen=10

	au InsertEnter * set timeout
	au InsertLeave * set notimeout
augroup END

" Define ' ' as map leader"
let mapleader = ' '
let g:mapleader = ' '

" Disable all bells"
set noerrorbells visualbell t_vb=

" Color column at 80
"set colorcolumn=80

" Wild menu (Autocompletion)"
set wildmenu
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.jpeg,*.png,*.xpm,*.gif
set wildmode=full

" Backup and Swap"
set nobackup
set nowritebackup
set noswapfile

" Search Options"
set hlsearch   " Highlight search
set incsearch  " Incremental search
set magic      " Set magic on, for regular expressions
set ignorecase " Searches are Non Case-sensitive
set smartcase

" FOLDING
set foldenable
set foldmethod=marker
set foldlevel=0
set foldcolumn=0

" Look and Feel settings
set background=light
set t_Co=16
colorscheme jellybeans
set encoding=utf-8
set guifont=DejaVu\ Sans\ Mono\ for\ Powerline

" Display extra whitespace
set list listchars=tab:»·,trail:·

" General UI Options"
set laststatus=2       " Always show the statusline
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

set showmatch          " Shows matching brackets when text indicator is over them
set scrolloff=5        " Show 5 lines of context around the cursor
set sidescrolloff=20
set lazyredraw         " The screen won't be redrawn unless actions took place
set cursorline
set scrolljump=10
set showcmd
set ttyfast            " Improves redrawing for newer computers
set pumheight=10
set diffopt+=context:3
set nostartofline      " when moving thru the lines, the cursor will try to stay in the previous columns

" LAYOUT / TEXT FORMATTING
" Formatting Options
set wrap	" Soft Wrap in all files, while hard wrap can be allow by filetype
set linebreak " It maintains the whole words when wrapping

" Indentation"
set autoindent
set cindent
set smartindent

" Tab Options"
set shiftwidth=4
set tabstop=4
set expandtab
"set smarttab

" Fix terminal timeout when pressing escape
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif


" MAPPINGS

" General
  " Clear search highlighting
  nnoremap <silent><leader>c :nohlsearch<CR>

  " Highlight the current line
  nnoremap <silent> <Leader>h ml:execute 'match Search /\%'.line('.').'l/'<CR>

  " Start BufExplorer
  nnoremap <silent><leader>l :BufExplorer<CR>

  " Start ctrlp.vim
  nnoremap <silent><leader>t :CtrlP<CR>

  " Paste
  let paste_mode = 0 " 0 = normal, 1 = paste

  func! Paste_on_off()
    if g:paste_mode == 0
      set paste
      let g:paste_mode = 1
    else
      set nopaste
      let g:paste_mode = 0
    endif
    return
  endfunc

  " Paste Mode <F3>
  nnoremap <silent> <F3> :call Paste_on_off()<CR>
  set pastetoggle=<F3>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" configure syntastic syntax checking to check on open as well as save
"let g:syntastic_check_on_open=1

	" Repurpose left and right arrow keys to move between the buffers
	nnoremap <silent> <Down>   :bn<CR>
	nnoremap <silent> <Up>  :bp<CR>

	" Indent visual selected code without unselecting
	" As seen in vimcasts.org
	vmap > >gv
	vmap < <gv
	vmap = =gv

  au BufRead,BufNewFile Capfile set filetype=ruby

" 3RD-PARTY PLUGINS SETTINGS

  " Buffer Explorer
  let g:bufExplorerDefaultHelp=0       " Do not show default help.
  let g:bufExplorerShowRelativePath=1  " Show relative paths.

  " NERD tree
    let g:NERDTreeChristmasTree = 1
    let g:NERDTreeCaseSensitiveSort = 1
    let g:NERDTreeQuitOnOpen = 1
    let g:NERDTreeWinPos = 'left'
    let g:NERDTreeWinSize = 50
    let g:NERDTreeShowBookmarks = 1
    let g:NERDTreeDirArrows=1
    map <F2> :NERDTreeToggle<CR>

  " Slime
  let g:slime_target = "tmux"
  let g:slime_paste_file = "$HOME/.slime_paste"

  " vim-smooth-scroll
  noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
  noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
  noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
  noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

  " SnipMate
  imap <C-J> <esc>a<Plug>snipMateNextOrTrigger
  smap <C-J> <Plug>snipMateNextOrTrigger

  " YouCompleteMe
  let g:ycm_filetype_blacklist = {}
  let g:ycm_complete_in_comments = 1
  let g:ycm_collect_identifiers_from_comments_and_strings = 1
  let g:ycm_seed_identifiers_with_syntax = 1
  let g:ycm_filetype_identifier_grouping = 0
  
  " fullscreen
  map <F9> <Esc>:call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 0)<CR> 

"key shortcuts for tabs like in Chrome with selecing some specifically.
map <C-S-]> gt
map <C-S-[> gT
map <C-1> 1gt
map <C-2> 2gt
map <C-3> 3gt
map <C-4> 4gt
map <C-5> 5gt
map <C-6> 6gt
map <C-7> 7gt
map <C-8> 8gt
map <C-9> 9gt
map <C-0> :tablast<CR>

"prevent weird resize/move issues:
set guioptions-=L

"airline enabling of powerline fonts
let g:airline_powerline_fonts = 1

set guioptions-=T  "remove toolbar

