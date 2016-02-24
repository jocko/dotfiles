set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/ctrlp.vim'
Plugin 'dyng/ctrlsf.vim'
Plugin 'morhetz/gruvbox'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'easymotion/vim-easymotion'
Plugin 'vim-ruby/vim-ruby'
Plugin 'moll/vim-bbye'
" Plugin 'bling/vim-airline'
" Plugin 'itchyny/lightline.vim'
Plugin 'tpope/vim-vinegar'
Plugin 'mbbill/undotree'
Plugin 'tpope/vim-commentary'
" Plugin 'sjl/vitality.vim'
Plugin 'szw/vim-tags'
Plugin 'kana/vim-textobj-user'
" Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'rhysd/vim-textobj-ruby'
Plugin 'rking/ag.vim'
Plugin 'tpope/vim-eunuch'

call vundle#end()
filetype plugin indent on
syntax enable

" let g:gruvbox_termcolors=16
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
set wildmode=longest:full,full
set scrolloff=5
set undodir=$HOME/.vim/undo
set undofile
set backspace=indent,eol,start
set incsearch
set hlsearch

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

nnoremap j gj
nnoremap k gk

autocmd FileType erlang setlocal expandtab shiftwidth=4
autocmd BufNewFile,BufRead rebar.config setlocal ft=erlang

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
map <leader>b :CtrlPBuffer<CR>
" Always open files in a new window
let g:ctrlp_switch_buffer = 0

" Ag

" CtrlP, meet ag.
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" Disable cache when using ag, because apparently ag is fast as a space fart.
let g:ctrlp_use_caching = 0

nmap     <C-F>f <Plug>CtrlSFPrompt
"vmap     <C-F>f <Plug>CtrlSFVwordPath
"vmap     <C-F>F <Plug>CtrlSFVwordExec
nmap     <C-F>n <Plug>CtrlSFCwordExec
"nmap     <C-F>p <Plug>CtrlSFPwordPath
"nnoremap <C-F>o :CtrlSFOpen<CR>
"nnoremap <C-F>t :CtrlSFToggle<CR>
"inoremap <C-F>t <Esc>:CtrlSFToggle<CR>

nnoremap <c-]> g<c-]>
vnoremap <c-]> g<c-]>

runtime! macros/matchit.vim

" let g:lightline = {
"       \ 'colorscheme': 'wombat',
"       \ }
" let g:lightline.active = {
"       \ 'left': [ [ 'paste' ],
"       \           [ 'readonly', 'filename', 'modified' ] ],
"       \ 'right': [ [ 'lineinfo' ],
"       \            [ 'percent' ],
"       \            [ 'fileformat', 'fileencoding', 'filetype' ] ] }

