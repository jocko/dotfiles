set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/ctrlp.vim'
Plugin 'rking/ag.vim'
Plugin 'morhetz/gruvbox'
Plugin 'easymotion/vim-easymotion'
Plugin 'vim-ruby/vim-ruby'
Plugin 'moll/vim-bbye'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-vinegar'
Plugin 'mbbill/undotree'

call vundle#end()
filetype plugin indent on
syntax enable

set background=dark
color gruvbox

set autoread
autocmd FocusGained,BufEnter * checktime

set tabstop=2 shiftwidth=2
set number
set expandtab
set ignorecase
set smartcase
set ttimeout
set ttimeoutlen=100
set hidden
set laststatus=2
set wildmenu
set wildmode=longest:full
set scrolloff=5
set undodir=$HOME/.vim/undo
set undofile

set list
set listchars=tab:▸\ ,eol:¬,trail:·
augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

let mapleader=" "

" Start of line
noremap H ^
" End of line
noremap L g_

" EasyMotion

" Disable default mappings
let g:EasyMotion_do_mapping = 0
" Navigation up/down
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

nnoremap <leader>w :w<CR>
nnoremap q :q<CR>
nnoremap <leader>q :Bdelete<CR>

" Creating splits
nnoremap <leader>sh :leftabove vnew<CR>
nnoremap <leader>sl :rightbelow vnew<CR>
" Navigating splits
map <C-h> <C-W>h
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-l> <C-W>l

" CtrlP

map <leader>o :CtrlP<CR>
map <leader>O :CtrlPBuffer<CR>
" Always open files in a new window
let g:ctrlp_switch_buffer = 0
