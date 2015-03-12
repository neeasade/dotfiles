set title
set hidden

" Auto remove all whitespace on :w
autocmd BufWritePre * :%s/\s\+$//e

" Numbers
set number
set numberwidth=3

"exe "set path=".expand("$PATH")

syntax enable
set nomodeline
set backspace=indent,eol,start                          " Backspace will delete EOL chars, as well as indents
set shortmess=atToOI                                    " To avoid the 'Hit Enter' prompts caused by the file messages
set history=1000
set undolevels=1000
set confirm
set updatetime=1500

" Define ' ' as map leader"
let mapleader = ' '
let g:mapleader = ' '

" Disable all bells"
set noerrorbells visualbell t_vb=

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
set background=dark
colorscheme base16-default
set t_Co=16
set encoding=utf-8

" Display extra whitespace
set list listchars=tab:»·,trail:·

" General UI Options"
set laststatus=2       " Always show the statusline
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

set showmatch          " Shows matching brackets when text indicator is over them
set scrolloff=5        " Show 5 lines of context around the cursor
set sidescrolloff=20
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



" Mappings
" Clear search highlighting
nnoremap <silent><leader>c :nohlsearch<CR>

" Highlight the current line
nnoremap <silent> <Leader>h ml:execute 'match Search /\%'.line('.').'l/'<CR>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Repurpose left and right arrow keys to move between the buffers
nnoremap <silent> <Down>   :bn<CR>
nnoremap <silent> <Up>  :bp<CR>

" Indent visual selected code without unselecting
" As seen in vimcasts.org
vmap > >gv
vmap < <gv
vmap = =gv

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
let g:Powerline_symbols = 'fancy'
set guioptions-=T  "remove toolbar

