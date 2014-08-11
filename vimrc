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
color gruvbox

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

" Experimental stuff
let g:loaded_golden_ratio = 1
set scrolloff=4
"autocmd FileType ruby compiler ruby
"autocmd FileType ruby
"      \ let b:start = executable('pry') ? 'pry -r "%:p"' : 'irb -r "%:p"' |
"      \ if expand('%') =~# '_test\.rb$' |
"      \   let b:dispatch = 'testrb %' |
"      \ elseif expand('%') =~# '_spec\.rb$' |
"      \   let b:dispatch = 'rspec %' |
"      \ elseif !exists('b:dispatch') |
"      \   let b:dispatch = 'ruby -wc %' |
"      \ endif
"nnoremap <silent> + :resize +5<CR>
"nnoremap <silent> - :resize -5<CR>
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

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
set undofile

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

let g:toggle_list_no_mappings = 1
"nmap <script> <silent> <leader>z :call ToggleQuickfixList()<CR>

let g:EasyMotion_do_mapping = 0

" Leader bindings
let mapleader=" "

nnoremap <leader>w :w<CR>
nnoremap q :q<CR>
nnoremap <leader>q :Bdelete<CR>
"nnoremap Q :qa<CR>
"nnoremap <leader>x :x<CR>

" Easy motion
" TODO Consider mapping these directly to f, t, / etc.
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline = 1

" Rspec
nnoremap <leader>rs :call RunCurrentSpecFile()<CR> "spec
nnoremap <leader>re :call RunNearestSpec()<CR> "example
nnoremap <leader>rr :call RunLastSpec()<CR> "rerun
nnoremap <leader>ra :call RunAllSpecs()<CR> "all

" Git
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gc :Gcommit<CR>

"nnoremap <leader><leader> <c-^>
"nnoremap <CR> :nohlsearch<cr>
"map <leader>y "*y
"nmap <silent> <leader>ev :belowright split $MYVIMRC<CR>
"nmap <silent> <leader>sv :so $MYVIMRC<CR>
nnoremap <leader>sh :leftabove vnew<CR>
nnoremap <leader>sl :rightbelow vnew<CR>
nnoremap <leader>sk :leftabove new<CR>
nnoremap <leader>sj :rightbelow new<CR>

map <leader>o :CtrlP<CR>
map <leader>O :CtrlPBuffer<CR>
"map <leader>gl :CtrlP lib<cr>
"map <leader>gs :CtrlP spec<cr>
