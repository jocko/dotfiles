set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/ctrlp.vim'
" Plugin 'morhetz/gruvbox'
Plugin 'vim-scripts/twilight'
Plugin 'w0ng/vim-hybrid'
Plugin 'chriskempson/base16-vim'
Plugin 'easymotion/vim-easymotion'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-vinegar'
Plugin 'mbbill/undotree'
Plugin 'tpope/vim-commentary'
Plugin 'kana/vim-textobj-user'
Plugin 'rhysd/vim-textobj-ruby'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-surround'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'godlygeek/tabular'
Plugin 'tkhren/vim-textobj-numeral'
Plugin 'Julian/vim-textobj-variable-segment'
Plugin 'wellle/targets.vim'
Plugin 'jpalardy/vim-slime'
Plugin 'plasticboy/vim-markdown'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-repeat'
Plugin 'elixir-lang/vim-elixir'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
Plugin 'guns/vim-sexp'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'python-mode/python-mode'

call vundle#end()
filetype plugin indent on
syntax enable

" set background=dark
" color gruvbox

set background=dark
color base16-tomorrow-night

" XXX Sprinkle fairy dust
if has("termguicolors")
  set termguicolors
endif

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

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

autocmd FileType clojure setlocal lispwords+=go-loop

set t_ZH=[3m
set t_ZR=[23m

let mapleader=" "
let maplocalleader=","

nnoremap j gj
nnoremap k gk

map <leader>y "*y

autocmd FileType erlang setlocal expandtab shiftwidth=4
autocmd BufNewFile,BufRead rebar.config setlocal ft=erlang

autocmd FileType ruby abbr <buffer> pry! require 'pry'; binding.pry
autocmd FileType ruby nnoremap <buffer> <LocalLeader>gd :Ack! 'def (self\.)?<cword>\('<CR>

autocmd FileType gitcommit setlocal spell

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" EasyMotion

" Disable default mappings
let g:EasyMotion_do_mapping = 0
" Navigation up/down
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
" map <Leader>s <Plug>(easymotion-s)

nnoremap <leader>w :w<CR>
" nnoremap q :q<CR>
nnoremap <leader>q :Bdelete<CR>

" CtrlP
map <leader>o :CtrlP<CR>
map <leader>b :CtrlPBuffer<CR>
" Always open files in a new window
let g:ctrlp_switch_buffer = 0

if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " We can disable cache when using ag
  let g:ctrlp_use_caching = 0
  let g:ackprg = 'ag --vimgrep --smart-case' 
endif
nnoremap <leader>a :Ack! ""<Left>

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

let g:textobj_numeral_no_default_key_mappings = 1

vmap an <Plug>(textobj-numeral-a)
omap an <Plug>(textobj-numeral-a)
" Mark occurrences
"nmap <Leader>n :let @/ = expand("<cword>")<CR>:let &hlsearch = &hlsearch<CR>

" nnoremap <Leader>u :UndotreeToggle<CR>
nnoremap U :UndotreeToggle<CR>
let g:undotree_SetFocusWhenToggle=1

let g:slime_python_ipython = 1
let g:slime_target = "tmux"

vmap <Leader>fs :!sqlformat -r -kupper -<CR>

let g:vim_markdown_folding_disabled = 1

let g:pymode_folding = 0
let g:pymode_lint_ignore = "E501"
let g:pymode_run = 0
let g:pymode_breakpoint = 0
set completeopt=menu
