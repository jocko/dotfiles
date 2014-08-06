set nocompatible

call pathogen#infect()
call pathogen#helptags()

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

let mapleader=" "

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
set wildmode=longest:full
set hlsearch
set incsearch
set ignorecase
set smartcase
set splitbelow
set splitright

set list
set listchars=tab:▸\ ,eol:¬
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

nnoremap <leader><leader> <c-^>
nnoremap <CR> :nohlsearch<cr>
map <leader>y "*y
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" Experimental stuff
map <leader>o :CtrlP<CR>
nnoremap <leader>w :w<CR>
map <leader>gl :CtrlP lib<cr>
map <leader>gs :CtrlP spec<cr>
set scrolloff=4
autocmd FileType ruby compiler ruby
autocmd FileType ruby
      \ let b:start = executable('pry') ? 'pry -r "%:p"' : 'irb -r "%:p"' |
      \ if expand('%') =~# '_test\.rb$' |
      \   let b:dispatch = 'testrb %' |
      \ elseif expand('%') =~# '_spec\.rb$' |
      \   let b:dispatch = 'rspec %' |
      \ elseif !exists('b:dispatch') |
      \   let b:dispatch = 'ruby -wc %' |
      \ endif

