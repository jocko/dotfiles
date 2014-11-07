set nocompatible

call pathogen#infect()
call pathogen#helptags()

set autoread
autocmd FocusGained,BufEnter * checktime

if !has("gui_running")
  let g:gruvbox_italic=0
endif
let g:gruvbox_termcolors=16
set background=dark
color twilight256

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

set tabstop=2 shiftwidth=2
set expandtab
set smarttab
set backspace=indent,eol,start
set ttimeout
set ttimeoutlen=100
set number
set hidden
set autoindent
set nrformats-=octal
set laststatus=2
set ruler
set showcmd
set wildmenu
set wildmode=longest,full
set incsearch
set ignorecase
set smartcase

set list
set listchars=tab:▸\ ,eol:¬,trail:·
augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

let mapleader=" "

" Experimental stuff
set scrolloff=5
"set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set undofile

au FileType Dockerfile set tabstop=8 softtabstop=8 shiftwidth=8 noexpandtab

function! s:StripWhitespace( line1, line2 )
  " Save the current search and cursor position
  let _s=@/
  let l = line(".")
  let c = col(".")

  " Strip the whitespace
  silent! execute ':' . a:line1 . ',' . a:line2 . 's/\s\+$//e'

  " Restore the saved search and cursor position
  let @/=_s
  call cursor(l, c)
endfunction
command! -range=% StripWhitespace call <SID>StripWhitespace( <line1>, <line2>)

function! My_TabComplete()
  let line = getline('.')                         " curline
  let substr = strpart(line, -1, col('.')+1)      " from start to cursor
  let substr = matchstr(substr, "[^ \t]*$")       " word till cursor
  if (strlen(substr)==0)                          " nothing to match on empty string
    return "\<tab>"
  endif
  let bool = match(substr, '\.')                  " position of period, if any
  if (bool==-1)
    return "\<C-X>\<C-P>"                         " existing text matching
  else
    return "\<C-X>\<C-U>"                         " plugin matching
  endif
endfunction
"inoremap <C-k> <C-R>=My_TabComplete()<CR>
"let g:rspec_command = "Dispatch bundle exec rspec -f d -c {spec}"
let g:rspec_command = "w | Dispatch rspec {spec}"

augroup ft_vim
  au!
  au FileType vim setlocal foldmethod=marker
augroup END

" EasyMotion {{{

" Disable default mappings
let g:EasyMotion_do_mapping = 0

map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)
" Note to self: cl is equivalent to s, and cc to S.
" nmap s <Plug>(easymotion-s)
" map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" map <Leader>h <Plug>(easymotion-linebackward)

" }}}

" The Silver Searcher {{{

if executable('ag')
  " CtrlP, meet ag.
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " Disable cache when using ag, because apparently ag is fast as a space fart.
  let g:ctrlp_use_caching = 0

  " TODO I think I want to do some leader mappings starting with 'a'
endif

" }}}

" Sneak {{{

" TODO

" }}}

"nnoremap <leader><leader> <c-^>
" Copy to system clipboard
map <leader>y "*y

nnoremap <leader>w :w<CR>
nnoremap q :q<CR>
nnoremap <leader>q :Bdelete<CR>

" Rspec {{{

nnoremap <leader>rs :call RunCurrentSpecFile()<CR>
nnoremap <leader>re :call RunNearestSpec()<CR>
nnoremap <leader>rr :call RunLastSpec()<CR>
nnoremap <leader>ra :call RunAllSpecs()<CR>

" }}}
" Git {{{

nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gc :Gcommit<CR>

" }}}
" Creating splits {{{

nnoremap <leader>sh :leftabove vnew<CR>
nnoremap <leader>sl :rightbelow vnew<CR>
nnoremap <leader>sk :leftabove new<CR>
nnoremap <leader>sj :rightbelow new<CR>

" }}}
" CtrlP {{{

" TODO I want an outline of the current file, and grepping

" TODO Not entirely happy with these
map <leader>o :CtrlP<CR>
map <leader>O :CtrlPBuffer<CR>
"map <leader>gl :CtrlP lib<cr>
"map <leader>gs :CtrlP spec<cr>

" Do not open stuff in my startify window
let g:ctrlp_reuse_window  = 'startify'

" Always open files in a new window
let g:ctrlp_switch_buffer = 0

" }}}

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

noremap H ^
noremap L g_

let g:startify_custom_header = [
      \ "               _|\\ _/|_,      ",
      \ "             ,((\\\\``-\\\\\\\\_    ",
      \ "           ,(())      `))\\    ",
      \ "         ,(()))       ,_ \\    ",
      \ "        ((())'   |        \\   ",
      \ "        )))))     >.__     \\  ",
      \ "        ((('     /    `-. .c| ",
      \ "                /        `-`' ",
      \ "",
      \]

highlight OverLength ctermbg=red ctermfg=white
match OverLength /\%120v.\+/

" TODO This does not restore cursor to previous position
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

