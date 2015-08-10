" Tab Options
set shiftwidth=4
set tabstop=4
" set expandtab "Enter spaces instead of tabs.

" {{{ Plugins
call plug#begin('~/.vim/plugged')
    Plug 'tpope/vim-sensible'                   " sensible defaults.
    Plug 'bling/vim-airline'                    " Status line
    Plug 'jeffkreeftmeijer/vim-numbertoggle'    " Auto relative number toggling
    Plug 'airblade/vim-gitgutter'               " Live git changes
    Plug 'tpope/vim-fugitive'                   " Complement git in vim - todo: learn this.
    Plug 'tpope/vim-sleuth'                     " Auto spacing/indenting conformity to files
    Plug 'terryma/vim-multiple-cursors'         " Muliple cursors - todo: keybinds/practice this.
    Plug 'Shougo/neocomplete.vim'               " Easily customized cached completion.
    Plug 'jiangmiao/auto-pairs'                 " auto-pairs(brackets/quotes)
    Plug 'scrooloose/nerdtree',                 {'on': 'NERDTreeToggle'} " Side panel file browser. todo: keybinds

    Plug 'davidhalter/jedi-vim',                {'for': 'python'} " python autocomplete - todo: keybinds for doc lookup/autopop revision.

    Plug 'mattn/emmet-vim',                     {'for': ['html', 'xml', 'xsl', 'xslt', 'xsd', 'css', 'sass', 'scss', 'less', 'mustache', 'php']}
    Plug 'Valloric/MatchTagAlways',             {'for': ['html', 'xhtml', 'xml', 'jinja']}

    Plug 'junegunn/fzf',                        {'dir': '~/fzf', 'do': 'yes\| ./install'} " based fuzzy search.

call plug#end()
" }}}

" {{{ Autocompletion
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*' " todo: look into this/that function w/ multiple cursors in daniels.
let g:neocomplete#sources#dictionary#dictionaries =  { 'default' : '' }
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()

" for jedi-vim + neocomplete(python):
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
if !exists('g:neocomplete#force_omni_input_patterns')
    let g:neocomplete#force_omni_input_patterns = {}
    let g:neocomplete#force_omni_input_patterns.python='\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
endif
" }}}

" airline specific setting:
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" todo: tab / shift-tab in highlight.

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
set wildmode=list:longest,full

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

" Colors
set t_Co=256
if empty(glob('~/.bspwm_theme'))
    colorscheme base16-twilight
else
    let vim_colors=system("cat ~/.bspwm_theme | grep VIM_COLORS | cut -c 12-")
    execute 'colorscheme ' vim_colors
endif

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
set smartindent

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Repurpose up and down arrow keys to move between the buffers
nnoremap <silent> <Down>   :bn<CR>
nnoremap <silent> <Up>  :bp<CR>

" Indent visual selected code without unselecting
" As seen in vimcasts.org
" todo - revaluate this when tab/shift-tab
vmap > >gv
vmap < <gv
vmap = =gv

"key shortcuts for tabs like in Chrome with selecing some specifically.
map <C-S-]> gt
map <C-S-[> gT

"copy visual
map <C-c> "+y<CR>

" gvim options - remove the toolbar.
set guioptions-=L
set guioptions-=T
