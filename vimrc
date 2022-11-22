set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
  call vundle#begin()

  Plugin 'VundleVim/Vundle.vim'

  " TODO Can I make a delete-list movement? Eg delete `bar,` or `baz` from (/[foo, bar, baz]/), what about maps/dicts?

  " TODO Group plugins into essential, ui-stuff (eg themes), nice-to-have
  " etc.  Also might be a good idea to describe a specific problem that
  " each one solves (i.e. motivation to why it is here). Ideally, plugins
  " should only have ui-stuff (I should be able to acomplish essential
  " tasks everywhere)

  " Git wrapper
  Plugin 'tpope/vim-fugitive'
  " Reopen files at last edit position
  Plugin 'farmergreg/vim-lastplace'
  " Automatically add end statements, e.g. in Ruby, end after def etc.
  Plugin 'tpope/vim-endwise.git'

  Plugin 'junegunn/fzf'
  Plugin 'junegunn/fzf.vim'

  " Themes
  Plugin 'morhetz/gruvbox'
  Plugin 'liuchengxu/space-vim-dark'
  Plugin 'arcticicestudio/nord-vim'

  " Nice to have stuff
  Plugin 'aquach/vim-http-client'
  Plugin 'jpalardy/vim-slime'

  Plugin 'vim-ruby/vim-ruby'

  Plugin 'pangloss/vim-javascript'
  Plugin 'leafgarland/typescript-vim'

  " TODO Can I get rid of this?
  " Plugin 'tpope/vim-vinegar'
  " TODO What are the 'native' alternatives to this?
  Plugin 'mileszs/ack.vim'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-surround'

  " TODO I do not grok how this plugin should be configured. Linting is
  " sluggish (at least for typescript) and I'm not that interested in it
  " (but how to turn off linting only?). Having prettier is nice, but
  " if I turn of linters, then I cannot do pretty, or C-]. Maybe doesn't
  " matter because prettier is super slow as well. I would want navigate
  " to tag (i.e. C-]) but maybe try ctags first, or find another plugin
  " that suits me better.
  " Plugin 'dense-analysis/ale'

  call vundle#end()

  " Colorscheme overrides. Since we do this with autocommands, it is
  " possible to try out new themes (e.g. :color gruvbox) and not have
  " these overrides carry over to the next theme . Also, the overrides
  " are scoped to a specific theme.
  autocmd ColorScheme nord highlight IncSearch cterm=NONE ctermfg=0 ctermbg=11
  autocmd ColorScheme gruvbox highlight IncSearch cterm=NONE ctermfg=0 ctermbg=9

  " Required for some themes (e.g. gruvbox)
  set background=dark

  color gruvbox
  " color nord


  " TODO When doing :Git blame for example, the commit hash for
  " uncommitted changes is represented by 000000 in a dark color which
  " sometimes makes it difficult to identify where the cursor is. Nord
  " for example has this problem. Maybe a good idea to make the cursor
  " stand out more? Disabling colors in fugitive, like this:
  "
  "   let g:fugitive_dynamic_colors = 0
  "
  " somewhat solves this problem. But colors are nice, and I want them.

  " let g:ale_fixers = {'typescriptreact': ['deno']}
  " let g:ale_linters = {'typescriptreact': ['deno']}
  let g:ale_linters_explicit = 1
endif

filetype plugin indent on
syntax enable

set path+=src/**
" set path-=/usr/include

" Automatically reread files that have changed outside vim
set autoread
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

" TODO What is this?
set t_ZH=[3m
set t_ZR=[23m

let mapleader=" "
let maplocalleader=","

" Clear search higlight
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" TODO Clean up
autocmd FileType typescript.tsx setlocal noexpandtab shiftwidth=2
autocmd FileType typescript setlocal noexpandtab shiftwidth=2
autocmd FileType typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType javascript.jsx setlocal noexpandtab shiftwidth=2
autocmd FileType javascript setlocal noexpandtab shiftwidth=2
autocmd FileType less setlocal noexpandtab shiftwidth=2
autocmd FileType scss setlocal noexpandtab shiftwidth=2

autocmd FileType gitcommit setlocal spell

" nnoremap <leader>w :w<CR>
" Use ma to set mark, then leader+a will jump back to it
" TODO Practice this instead of having a mapping
" nnoremap <leader>a `a

map <leader>o :Files<CR>
" map <leader>O :GFiles?<CR>
" map <leader>b :Buffers<CR>

if executable('ag')
  let $FZF_DEFAULT_COMMAND = 'ag -g ""'
  let g:ackprg = 'ag --vimgrep --smart-case' 
endif
nnoremap <leader>s :Ack! ""<Left>

" TODO Need this?
" Switch to other window
" nnoremap <leader><Tab> <c-w>w
" Or, the same thing, but emacs style. The jury is still out...
" nnoremap <c-x>o <c-w>p

" Jump to tag, show list if multiple tags
" TODO Need this?
" nnoremap <c-]> g<c-]>
" vnoremap <c-]> g<c-]>

" TODO What is this?
runtime! macros/matchit.vim

" Mark occurrences (note: <raise>-f => *)
nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>

let g:slime_target = "vimterminal"

