set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/ctrlp.vim'
Plugin 'morhetz/gruvbox'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-vinegar'
Plugin 'mbbill/undotree'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'jpalardy/vim-slime'
Plugin 'elixir-lang/vim-elixir'
Plugin 'mileszs/ack.vim'
Plugin 'farmergreg/vim-lastplace'
Plugin 'kana/vim-textobj-user'
Plugin 'rhysd/vim-textobj-ruby'
Plugin 'tpope/vim-eunuch'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
Plugin 'guns/vim-sexp'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-endwise.git'
Plugin 'tpope/vim-unimpaired'

" vim-addon-mw-utils & tlib_vim are requried by vim-snipmate
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'

Plugin 'chakrit/upstart.vim'
Plugin 'Vimjas/vim-python-pep8-indent'

" TODO These three plugins, not sure if I'm using.
" Plugin 'tkhren/vim-textobj-numeral'
" Plugin 'Julian/vim-textobj-variable-segment'
" Plugin 'wellle/targets.vim'

call vundle#end()
filetype plugin indent on
syntax enable

set background=dark
color gruvbox

" set background=dark
" color base16-tomorrow-night

" XXX Sprinkle fairy dust
if has("termguicolors")
  set termguicolors
endif

set autoread
autocmd FocusGained,BufEnter * checktime

set tabstop=2 shiftwidth=2
set number
set expandtab
" set ignorecase
" set smartcase
set ttimeout
set ttimeoutlen=100
set hidden
set laststatus=2
set wildmenu
set wildmode=longest:full,full
set wildignore+=venv,.git,*.orig
set scrolloff=5
set undofile
set backspace=indent,eol,start
set incsearch
set hlsearch
set relativenumber
set completeopt=longest,menu,preview
set wrap
" I sometimes get this strange behavior where the cursor jumps forward and
" then back. Seem to be because of this.
" set lazyredraw

set list
set listchars=tab:▸\ ,eol:¬,trail:·
augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

set undodir=$HOME/.vim/undo
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

autocmd FileType clojure setlocal lispwords+=go-loop

set t_ZH=[3m
set t_ZR=[23m

let mapleader=" "
" TODO Find another localleader
let maplocalleader=","

" gj/gk operates on display lines (useful when 'wrap' is on)
" nnoremap j gj
" nnoremap k gk

" Clear search higlight
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" TODO Should learn to do this myself
" map <leader>y "*y

autocmd FileType erlang setlocal expandtab shiftwidth=4
autocmd BufNewFile,BufRead rebar.config setlocal ft=erlang

let g:ruby_indent_block_style = 'do'
" autocmd FileType ruby abbr <buffer> pry! require 'pry'; binding.pry
" autocmd FileType ruby nnoremap <buffer> <LocalLeader>gd :Ack! 'def (self\.)?<cword>\('<CR>

autocmd FileType gitcommit setlocal spell

nnoremap <leader>w :w<CR>
" Use ma to set mark, then leader+a will jump back to it
nnoremap <leader>a `a

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
" nnoremap <leader>a :Ack! ""<Left>

" Switch to other window
nnoremap <leader><Tab> <c-w>w
" Or, the same thing, but emacs style. The jury is still out...
nnoremap <c-x>o <c-w>w

" Jump to tag, show list if multiple tags
nnoremap <c-]> g<c-]>
vnoremap <c-]> g<c-]>

runtime! macros/matchit.vim

let g:textobj_numeral_no_default_key_mappings = 1

vmap an <Plug>(textobj-numeral-a)
omap an <Plug>(textobj-numeral-a)

" Mark occurrences (note: <raise>-f => *)
nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>

" nnoremap <Leader>u :UndotreeToggle<CR>
nnoremap U :UndotreeToggle<CR>
let g:undotree_SetFocusWhenToggle=1

let g:slime_python_ipython = 1
let g:slime_target = "tmux"

" vmap <Leader>fs :!sqlformat -r -kupper -<CR>

" let g:vim_markdown_folding_disabled = 1

let g:pymode_folding = 0
let g:pymode_lint_ignore = "E501"
let g:pymode_run = 0
let g:pymode_breakpoint = 0
