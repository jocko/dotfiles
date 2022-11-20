set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" TODO mappings that are nice, but always forget
" * make split equal (ie after resize) C-w =
" * center view at current line

" TODO Can I make a delete-list movement? Eg delete `bar,` or `baz` from (/[foo, bar, baz]/), what about maps/dicts?
" TODO Would be nice to be able to evaluate arbitrary code in vim, eg send
" selection to node and display result
" TODO Native alternative to fzf?

" TODO Group plugins into essential, ui-stuff (eg themes), nice-to-have
" etc.  Also might be a good idea to describe a specific problem that
" each one solves (i.e. motivation to why it is here). Ideally, plugins
" should only have ui-stuff (I should be able to acomplish essential
" tasks everywhere)

" Themes
Plugin 'morhetz/gruvbox'
Plugin 'liuchengxu/space-vim-dark'
Plugin 'arcticicestudio/nord-vim'

Plugin 'vim-ruby/vim-ruby'
" Enhances Vims built-in file browser, netrw. TODO Learn more about what this actually does
Plugin 'tpope/vim-vinegar'
" TODO Remove
" Plugin 'mbbill/undotree'
"
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
" TODO What are the 'native' alternatives to this?
Plugin 'mileszs/ack.vim'
Plugin 'farmergreg/vim-lastplace'
" TODO Remove
Plugin 'kana/vim-textobj-user'
Plugin 'rhysd/vim-textobj-ruby'
Plugin 'tpope/vim-eunuch'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-fireplace'
"
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-endwise.git'
" TODO Remove
Plugin 'tpope/vim-unimpaired'
"
Plugin 'pangloss/vim-javascript'
Plugin 'leafgarland/typescript-vim'
" TODO What does this do?
Plugin 'vim-syntastic/syntastic'

Plugin 'maxmellon/vim-jsx-pretty'
Plugin 'prettier/vim-prettier'
" TODO Remove
Plugin 'wellle/targets.vim'
"

" TODO Remove
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'bps/vim-textobj-python'
"

Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'

Plugin 'aquach/vim-http-client'

call vundle#end()

filetype plugin indent on
syntax enable

" TODO Only if plugin have been installed
color nord

" Makes :fin[d]  look for files recursively
" TODO Should i remove the /usr/include default entry?
set path+=**

" Automatically reread files that have changed outside vim
set autoread
" TODO Is this still needed? If so, try to document why
autocmd FocusGained,BufEnter * checktime

set tabstop=2 shiftwidth=2
set expandtab
set number
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
" This prevents CTRL-A & CTRL-X to interpret numbers with leading zeroes as octal
set nrformats-=octal
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

" TODO plugin stuff, move
let g:http_client_focus_output_window = 0
let g:http_client_result_vsplit = 0
let g:http_client_preserve_responses = 1

set undodir=~/.vim/undo
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set directory=~/.vim/swap//

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

autocmd FileType clojure setlocal lispwords+=go-loop

set t_ZH=[3m
set t_ZR=[23m

let mapleader=" "
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

" autocmd FileType c setlocal noexpandtab shiftwidth=4

autocmd FileType typescript.tsx setlocal noexpandtab shiftwidth=2
autocmd FileType typescript setlocal noexpandtab shiftwidth=2
autocmd FileType typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType javascript.jsx setlocal noexpandtab shiftwidth=2
autocmd FileType javascript setlocal noexpandtab shiftwidth=2
autocmd FileType less setlocal noexpandtab shiftwidth=2
autocmd FileType scss setlocal noexpandtab shiftwidth=2

let g:ruby_indent_block_style = 'do'
" autocmd FileType ruby abbr <buffer> pry! require 'pry'; binding.pry
" autocmd FileType ruby nnoremap <buffer> <LocalLeader>gd :Ack! 'def (self\.)?<cword>\('<CR>
autocmd FileType ruby nnoremap <buffer> <LocalLeader>s :execute "SlimeSend1 load('".expand('%:p')."')"<CR>

autocmd FileType gitcommit setlocal spell

" nnoremap <leader>w :w<CR>
" Use ma to set mark, then leader+a will jump back to it
" TODO Practice this instead of having a mapping
" nnoremap <leader>a `a

map <leader>o :Files<CR>
map <leader>O :GFiles?<CR>
map <leader>b :Buffers<CR>

if executable('ag')
  let $FZF_DEFAULT_COMMAND = 'ag -g ""'
  let g:ackprg = 'ag --vimgrep --smart-case' 
endif
nnoremap <leader>s :Ack! ""<Left>

" Switch to other window
nnoremap <leader><Tab> <c-w>w
" Or, the same thing, but emacs style. The jury is still out...
nnoremap <c-x>o <c-w>p

" Jump to tag, show list if multiple tags
nnoremap <c-]> g<c-]>
vnoremap <c-]> g<c-]>

runtime! macros/matchit.vim

let g:textobj_numeral_no_default_key_mappings = 1

vmap an <Plug>(textobj-numeral-a)
omap an <Plug>(textobj-numeral-a)

" Mark occurrences (note: <raise>-f => *)
nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>

let g:slime_python_ipython = 1
let g:slime_target = "tmux"
" Eval code (w. slime) as indicated by motion.
autocmd FileType python nmap <buffer> cp <Plug>SlimeMotionSend

" autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])

