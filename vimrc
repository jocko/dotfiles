filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
  call vundle#begin()

  Plugin 'VundleVim/Vundle.vim'

  Plugin 'tpope/vim-fugitive'

  " TODO Probably use httpie + slime instead
  Plugin 'aquach/vim-http-client'
  Plugin 'jpalardy/vim-slime'
  Plugin 'ludovicchabant/vim-gutentags'

  " Themes
  Plugin 'morhetz/gruvbox'
  Plugin 'liuchengxu/space-vim-dark'
  Plugin 'arcticicestudio/nord-vim'


  " Language specific stuff
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'leafgarland/typescript-vim'
  Plugin 'maxmellon/vim-jsx-pretty'

  " Automatically add end statements. `end` after `def` in Ruby for example
  Plugin 'tpope/vim-endwise.git'
  " Comment stuff out
  Plugin 'tpope/vim-commentary'

  " TODO Evaluate
  Plugin 'vim-pandoc/vim-pandoc'
  Plugin 'vim-pandoc/vim-pandoc-syntax'

  call vundle#end()
endif

filetype plugin indent on
syntax enable

" Colorscheme overrides, need to appear before we source any
" colorscheme. Since we do this with autocommands, it is possible to
" try out new themes (e.g. :color gruvbox) and not have these overrides
" carry over to the next theme . Also, the overrides are scoped to a
" specific theme.
autocmd ColorScheme nord highlight IncSearch cterm=NONE ctermfg=0 ctermbg=11
autocmd ColorScheme gruvbox highlight IncSearch cterm=NONE ctermfg=0 ctermbg=9

if filereadable(expand("~/.vim/bundle/gruvbox/README.md"))
  " Required for some themes (e.g. gruvbox)
  set background=dark
  color gruvbox
endif

" Automatically reread files that have changed outside vim
set autoread
augroup auto_checktime
  autocmd!
  " https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
  " https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
  autocmd FocusGained,BufEnter,CursorHold,CursorHoldI *
          \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif
augroup END

" Jump to the last known cursor position
augroup lastplace
  au!
  autocmd BufReadPost *
        \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
        \ |   exe "normal! g`\""
        \ | endif
augroup END

set tabstop=2 shiftwidth=2
set expandtab
set number
set ttimeout
set ttimeoutlen=100
set hidden
set laststatus=2
set wildmenu
set wildmode=longest:full,full
set wildignorecase
set wildoptions=pum,fuzzy
set ignorecase
set smartcase
set tagcase=match
set scrolloff=5
set undofile
set backspace=indent,eol,start
set incsearch
" set hlsearch
set relativenumber
set completeopt=longest,menu,preview
" This is useful, but throws off j/k movement with count (if there are any
" wrapped lines between). See gj/gk which operates on display lines instead.
set wrap
set nrformats=bin,hex,unsigned
set cursorline
set list
set listchars=tab:▸\ ,trail:·
augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

" Plugin config
let g:http_client_focus_output_window = 0
let g:http_client_result_vsplit = 0
let g:http_client_preserve_responses = 1
let g:slime_target = "vimterminal"
let g:pandoc#modules#disabled = ["folding"]
let g:gutentags_define_advanced_commands = 1
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root = ['package.json', 'Gemfile']
let g:gutentags_file_list_command = {
            \ 'markers': {
            \ 'package.json': 'ack -f --no-filter -t ts -t js',
            \ 'Gemfile': 'ack -f --no-filter -t ruby',
            \ },
            \ }
let g:gutentags_ctags_exclude_wildignore = 0
let g:gutentags_ctags_exclude = []
let g:gutentags_ctags_exclude += ['*.test.ts']
let g:gutentags_ctags_extra_args = []
let g:gutentags_ctags_extra_args += ['--TypeScript-kinds=-p']

set undodir=~/.vim/undo
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set directory=~/.vim/swap//
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif

let mapleader=" "

runtime! macros/matchit.vim

" Navigate prev/next quickfix errors
nnoremap <silent> [a :previous<CR>
nnoremap <silent> ]a :next<CR>
nnoremap <silent> [A :first<CR>
nnoremap <silent> ]A :last<CR>
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]q :cnext<CR>
nnoremap <silent> [Q :cfirst<CR>
nnoremap <silent> ]Q :clast<CR>

