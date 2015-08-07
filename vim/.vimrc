
" Plugins
call plug#begin('~/.vim/plugged')
    Plug 'tpope/vim-sensible'                   " sensible defaults.
    Plug 'bling/vim-airline'                    " Status line
    Plug 'jeffkreeftmeijer/vim-numbertoggle'    " Auto relative number toggling
    Plug 'airblade/vim-gitgutter'               " Live git changes
    Plug 'tpope/vim-fugitive'                   " Complement git in vim
    Plug 'tpope/vim-sleuth'                     " Auto spacing/indenting conformity to files
    Plug 'kien/ctrlp.vim'                       " fuzzy file, buffer, tag finder
    Plug 'davidhalter/jedi-vim',                {'for': 'python'} " python autocomplete
call plug#end()

" airline specific setting:
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" Tab Options
" set shiftwidth=4
" set tabstop=4
" set expandtab "Inter spaces instead of tabs.
" todo: shift-tab

" set title and allow hidden buffers
set title
set hidden

" Auto remove all trailing whitespace on :w
autocmd BufWritePre * :%s/\s\+$//e

" Autosave files when focus is lost
:au FocusLost * :wa

" Line Numbers
set number
set numberwidth=3

" Path will be base dir that vim is opened from
set path=$PWD/**

set nomodeline
set noshowmode                                          " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set shortmess=atToOI                                    " To avoid the 'Hit Enter' prompts caused by the file messages
set undolevels=1000
set updatetime=1500

" Define ' ' as map leader
let mapleader = ' '
let g:mapleader = ' '

" Disable all bells
" set noerrorbells visualbell t_vb=

" Wild menu (Autocompletion)"
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.jpeg,*.png,*.xpm,*.gif

" todo: understand this:
set wildmode=full

" Backup and Swap
set nobackup
set nowritebackup
set noswapfile

" Search Options
set hlsearch   " Highlight search
set magic      " Set magic on, for regular expressions
set ignorecase " Searches are Non Case-sensitive
set smartcase

" FOLDING
set foldenable
set foldmethod=marker
set foldlevel=0
set foldcolumn=0

" Colors - todo - default if file not found.
let vim_colors=system("cat ~/.bspwm_theme | grep VIM_COLORS | cut -c 12-")
execute 'colorscheme ' vim_colors
set t_Co=256

set encoding=utf-8

" General UI Options
set mouse=a
set showmatch          " Shows matching brackets when text indicator is over them
set cursorline
set scrolljump=10
set ttyfast            " Improves redrawing for newer computers
set pumheight=20       " popup menu height
set diffopt+=context:3
set nostartofline      " when moving thru the lines, the cursor will try to stay in the previous columns

" LAYOUT / TEXT FORMATTING
" Formatting Options
set wrap " Soft Wrap in all files, while hard wrap can be allow by filetype
set linebreak " It maintains the whole words when wrapping

" todo : see if needed.
set smartindent

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Repurpose up and down arrow keys to move between the buffers
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

" sync yanking and pasting with clipboard
" todo: revisit this setting.
set clipboard=unnamedplus

"copy visual
map <C-c> "+y<CR>

" gvim options - remove the toolbar.
set guioptions-=L
set guioptions-=T
