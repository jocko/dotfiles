filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
  call vundle#begin()

  Plugin 'VundleVim/Vundle.vim'

  " TODO Can I make a delete-list movement? Eg delete `bar,` or `baz` from (/[foo, bar, baz]/), what about maps/dicts?

  " Git wrapper
  Plugin 'tpope/vim-fugitive'
  " Reopen files at last edit position
  " TODO I believe this can be done without plugins
  Plugin 'farmergreg/vim-lastplace'
  " Automatically add end statements. `end` after `def` in Ruby for example
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

  " Language specific stuff
  Plugin 'vim-ruby/vim-ruby'

  " TODO Figure out this JS/TypeScript stuff. What do I need here????
  " Plugin 'pangloss/vim-javascript'
  Plugin 'leafgarland/typescript-vim'
  " Plugin 'yuezk/vim-js'
  " Makes indentation bueno (for typescript at least)
  " Plugin 'maxmellon/vim-jsx-pretty'

  " TODO What are the 'native' alternatives to this?
  " Plugin 'mileszs/ack.vim'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-surround'

  " TODO Evaluate
  Plugin 'vim-pandoc/vim-pandoc'
  Plugin 'vim-pandoc/vim-pandoc-syntax'
  Plugin 'ludovicchabant/vim-gutentags'

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
" autocmd FocusGained,BufEnter * if mode() != 'c' | checktime | endif
augroup auto_checktime
  autocmd!
  " Notify if file is changed outside of vim
  " Trigger `checktime` when files changes on disk
  " https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
  " https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
  autocmd FocusGained,BufEnter,CursorHold,CursorHoldI *
          \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif
augroup END

set tabstop=4 shiftwidth=4
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
" TODO Evaluate ignorecase/smartcase
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
let g:gutentags_project_root = ['package.json']
let g:gutentags_file_list_command = {
            \ 'markers': {
            \ 'package.json': 'ack -f --no-filter -t ts -t js',
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

" TODO Clean up
" autocmd FileType typescript.tsx setlocal noexpandtab shiftwidth=2
" autocmd FileType typescript setlocal noexpandtab shiftwidth=2
" autocmd FileType typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
" autocmd FileType javascript.jsx setlocal noexpandtab shiftwidth=2
" autocmd FileType javascript setlocal noexpandtab shiftwidth=2
" autocmd FileType less setlocal noexpandtab shiftwidth=2
" autocmd FileType scss setlocal noexpandtab shiftwidth=2
" autocmd FileType sh setlocal expandtab tabstop=2 shiftwidth=2

runtime! macros/matchit.vim

" This makes * stay on the word instead of jumping to the next one.
" TODO But seem to clash with my auto clearing hlsearch stuff
" nnoremap * *N

" TODO goto last/first
" Navigate prev/next quickfix errors
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]q :cnext<CR>
" Clear search higlight
" nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" Clear hlsearch automatically
" augroup vimrc-incsearch-highlight
"   autocmd!
"   autocmd CmdlineEnter [/\?] :set hlsearch
"   autocmd CmdlineLeave [/\?] :set nohlsearch
" augroup END
