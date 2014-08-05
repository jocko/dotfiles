set nocompatible

call pathogen#infect()
call pathogen#helptags()

if !has("gui_running")
  let g:gruvbox_italic=0
endif
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

let g:neocomplete#enable_at_startup = 1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

