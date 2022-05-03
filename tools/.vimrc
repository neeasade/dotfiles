" sensible defaults by tpope on 5-10-2018:
" this is a masshup of vim sleuth, sensible defaults, commentery, and my own stuff (at the bottom)

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab
set nrformats-=octal

if !has('nvim') && &ttimeoutlen == -1
  set ttimeout
  set ttimeoutlen=100
endif

set incsearch
" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

set laststatus=2
set ruler
set wildmenu

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
  set shell=/bin/bash
endif

set autoread

set history=1000
set tabpagemax=50
set viminfo^=!
set sessionoptions-=options

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux\|^Eterm'
  set t_Co=16
endif

runtime! macros/matchit.vim
inoremap <C-U> <C-G>u<C-U>

" vim sleuth by the same, all hail vim jesus:
function! s:guess(lines) abort
  let options = {}
  let heuristics = {'spaces': 0, 'hard': 0, 'soft': 0}
  let ccomment = 0
  let podcomment = 0
  let triplequote = 0
  let backtick = 0
  let xmlcomment = 0
  let softtab = repeat(' ', 8)

  for line in a:lines
    if !len(line) || line =~# '^\s*$'
      continue
    endif

    if line =~# '^\s*/\*'
      let ccomment = 1
    endif
    if ccomment
      if line =~# '\*/'
        let ccomment = 0
      endif
      continue
    endif

    if line =~# '^=\w'
      let podcomment = 1
    endif
    if podcomment
      if line =~# '^=\%(end\|cut\)\>'
        let podcomment = 0
      endif
      continue
    endif

    if triplequote
      if line =~# '^[^"]*"""[^"]*$'
        let triplequote = 0
      endif
      continue
    elseif line =~# '^[^"]*"""[^"]*$'
      let triplequote = 1
    endif

    if backtick
      if line =~# '^[^`]*`[^`]*$'
        let backtick = 0
      endif
      continue
    elseif line =~# '^[^`]*`[^`]*$'
      let backtick = 1
    endif

    if line =~# '^\s*<\!--'
      let xmlcomment = 1
    endif
    if xmlcomment
      if line =~# '-->'
        let xmlcomment = 0
      endif
      continue
    endif

    if line =~# '^\t'
      let heuristics.hard += 1
    elseif line =~# '^' . softtab
      let heuristics.soft += 1
    endif
    if line =~# '^  '
      let heuristics.spaces += 1
    endif
    let indent = len(matchstr(substitute(line, '\t', softtab, 'g'), '^ *'))
    if indent > 1 && get(options, 'shiftwidth', 99) > indent
      let options.shiftwidth = indent
    endif
  endfor

  if heuristics.hard && !heuristics.spaces
    return {'expandtab': 0, 'shiftwidth': &tabstop}
  elseif heuristics.soft != heuristics.hard
    let options.expandtab = heuristics.soft > heuristics.hard
    if heuristics.hard
      let options.tabstop = 8
    endif
  endif

  return options
endfunction

function! s:patterns_for(type) abort
  if a:type ==# ''
    return []
  endif
  if !exists('s:patterns')
    redir => capture
    silent autocmd BufRead
    redir END
    let patterns = {
          \ 'c': ['*.c'],
          \ 'html': ['*.html'],
          \ 'sh': ['*.sh'],
          \ 'vim': ['vimrc', '.vimrc', '_vimrc'],
          \ }
    let setfpattern = '\s\+\%(setf\%[iletype]\s\+\|set\%[local]\s\+\%(ft\|filetype\)=\|call SetFileTypeSH(["'']\%(ba\|k\)\=\%(sh\)\@=\)'
    for line in split(capture, "\n")
      let match = matchlist(line, '^\s*\(\S\+\)\='.setfpattern.'\(\w\+\)')
      if !empty(match)
        call extend(patterns, {match[2]: []}, 'keep')
        call extend(patterns[match[2]], [match[1] ==# '' ? last : match[1]])
      endif
      let last = matchstr(line, '\S.*')
    endfor
    let s:patterns = patterns
  endif
  return copy(get(s:patterns, a:type, []))
endfunction

function! s:apply_if_ready(options) abort
  if !has_key(a:options, 'expandtab') || !has_key(a:options, 'shiftwidth')
    return 0
  else
    for [option, value] in items(a:options)
      call setbufvar('', '&'.option, value)
    endfor
    return 1
  endif
endfunction

function! s:detect() abort
  if &buftype ==# 'help'
    return
  endif

  let options = s:guess(getline(1, 1024))
  if s:apply_if_ready(options)
    return
  endif
  let c = get(b:, 'sleuth_neighbor_limit', get(g:, 'sleuth_neighbor_limit', 20))
  let patterns = c > 0 ? s:patterns_for(&filetype) : []
  call filter(patterns, 'v:val !~# "/"')
  let dir = expand('%:p:h')
  while isdirectory(dir) && dir !=# fnamemodify(dir, ':h') && c > 0
    for pattern in patterns
      for neighbor in split(glob(dir.'/'.pattern), "\n")[0:7]
        if neighbor !=# expand('%:p') && filereadable(neighbor)
          call extend(options, s:guess(readfile(neighbor, '', 256)), 'keep')
          let c -= 1
        endif
        if s:apply_if_ready(options)
          let b:sleuth_culprit = neighbor
          return
        endif
        if c <= 0
          break
        endif
      endfor
      if c <= 0
        break
      endif
    endfor
    let dir = fnamemodify(dir, ':h')
  endwhile
  if has_key(options, 'shiftwidth')
    return s:apply_if_ready(extend({'expandtab': 1}, options))
  endif
endfunction

setglobal smarttab

if !exists('g:did_indent_on')
  filetype indent on
endif

function! SleuthIndicator() abort
  let sw = &shiftwidth ? &shiftwidth : &tabstop
  if &expandtab
    return 'sw='.sw
  elseif &tabstop == sw
    return 'ts='.&tabstop
  else
    return 'sw='.sw.',ts='.&tabstop
  endif
endfunction

augroup sleuth
  autocmd!
  autocmd FileType *
        \ if get(b:, 'sleuth_automatic', get(g:, 'sleuth_automatic', 1))
        \ | call s:detect() | endif
  autocmd User Flags call Hoist('buffer', 5, 'SleuthIndicator')
augroup END

command! -bar -bang Sleuth call s:detect()

" commentary.vim - Comment stuff out
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.3
" GetLatestVimScripts: 3695 1 :AutoInstall: commentary.vim

if exists("g:loaded_commentary") || v:version < 700
  finish
endif
let g:loaded_commentary = 1

function! s:surroundings() abort
  return split(get(b:, 'commentary_format', substitute(substitute(substitute(
        \ &commentstring, '^$', '%s', ''), '\S\zs%s',' %s', '') ,'%s\ze\S', '%s ', '')), '%s', 1)
endfunction

function! s:strip_white_space(l,r,line) abort
  let [l, r] = [a:l, a:r]
  if l[-1:] ==# ' ' && stridx(a:line,l) == -1 && stridx(a:line,l[0:-2]) == 0
    let l = l[:-2]
  endif
  if r[0] ==# ' ' && a:line[-strlen(r):] != r && a:line[1-strlen(r):] == r[1:]
    let r = r[1:]
  endif
  return [l, r]
endfunction

function! s:go(...) abort
  if !a:0
    let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
    return 'g@'
  elseif a:0 > 1
    let [lnum1, lnum2] = [a:1, a:2]
  else
    let [lnum1, lnum2] = [line("'["), line("']")]
  endif

  let [l, r] = s:surroundings()
  let uncomment = 2
  for lnum in range(lnum1,lnum2)
    let line = matchstr(getline(lnum),'\S.*\s\@<!')
    let [l, r] = s:strip_white_space(l,r,line)
    if len(line) && (stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
      let uncomment = 0
    endif
  endfor

  if get(b:, 'commentary_startofline')
    let indent = '^'
  else
    let indent = '^\s*'
  endif

  for lnum in range(lnum1,lnum2)
    let line = getline(lnum)
    if strlen(r) > 2 && l.r !~# '\\'
      let line = substitute(line,
            \'\M' . substitute(l, '\ze\S\s*$', '\\zs\\d\\*\\ze', '') . '\|' . substitute(r, '\S\zs', '\\zs\\d\\*\\ze', ''),
            \'\=substitute(submatch(0)+1-uncomment,"^0$\\|^-\\d*$","","")','g')
    endif
    if uncomment
      let line = substitute(line,'\S.*\s\@<!','\=submatch(0)[strlen(l):-strlen(r)-1]','')
    else
      let line = substitute(line,'^\%('.matchstr(getline(lnum1),indent).'\|\s*\)\zs.*\S\@<=','\=l.submatch(0).r','')
    endif
    call setline(lnum,line)
  endfor
  let modelines = &modelines
  try
    set modelines=0
    silent doautocmd User CommentaryPost
  finally
    let &modelines = modelines
  endtry
  return ''
endfunction

function! s:textobject(inner) abort
  let [l, r] = s:surroundings()
  let lnums = [line('.')+1, line('.')-2]
  for [index, dir, bound, line] in [[0, -1, 1, ''], [1, 1, line('$'), '']]
    while lnums[index] != bound && line ==# '' || !(stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
      let lnums[index] += dir
      let line = matchstr(getline(lnums[index]+dir),'\S.*\s\@<!')
      let [l, r] = s:strip_white_space(l,r,line)
    endwhile
  endfor
  while (a:inner || lnums[1] != line('$')) && empty(getline(lnums[0]))
    let lnums[0] += 1
  endwhile
  while a:inner && empty(getline(lnums[1]))
    let lnums[1] -= 1
  endwhile
  if lnums[0] <= lnums[1]
    execute 'normal! 'lnums[0].'GV'.lnums[1].'G'
  endif
endfunction

command! -range -bar Commentary call s:go(<line1>,<line2>)
xnoremap <expr>   <Plug>Commentary     <SID>go()
nnoremap <expr>   <Plug>Commentary     <SID>go()
nnoremap <expr>   <Plug>CommentaryLine <SID>go() . '_'
onoremap <silent> <Plug>Commentary        :<C-U>call <SID>textobject(get(v:, 'operator', '') ==# 'c')<CR>
nnoremap <silent> <Plug>ChangeCommentary c:<C-U>call <SID>textobject(1)<CR>
nmap <silent> <Plug>CommentaryUndo :echoerr "Change your <Plug>CommentaryUndo map to <Plug>Commentary<Plug>Commentary"<CR>

if !hasmapto('<Plug>Commentary') || maparg('gc','n') ==# ''
  xmap gc  <Plug>Commentary
  nmap gc  <Plug>Commentary
  omap gc  <Plug>Commentary
  nmap gcc <Plug>CommentaryLine
  if maparg('c','n') ==# '' && !exists('v:operator')
    nmap cgc <Plug>ChangeCommentary
  endif
  nmap gcu <Plug>Commentary<Plug>Commentary
endif

" my changes:

" changing cursor
if $TERM !~# '^linux\|^Eterm'
  let &t_SI = "\<Esc>[6 q"
  let &t_SR = "\<Esc>[4 q"
  let &t_EI = "\<Esc>[2 q"
endif

set ignorecase
set clipboard=unnamedplus

" no visual bell
set t_vb=

" allow unsaved buffers to not show
set hidden
set hlsearch

set clipboard=unnamed

" qwerty
" imap fj <ESC>
" vmap fj <ESC>

" colemak
imap tn <ESC>
vmap tn <ESC>

noremap e k
noremap E K

noremap k n
noremap K N

noremap n j
noremap N J

noremap j e
noremap J E

" tell vim to keep a backup file
set backup
silent !mkdir ~/.vim_backup > /dev/null 2>&1

" tell vim where to put its backup files
set backupdir=~/.vim_backup

" tell vim where to put swap files
set dir=~/.vim_backup

" vim:set ft=vim et sw=2:
