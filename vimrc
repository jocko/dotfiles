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
  Plugin 'pangloss/vim-javascript'
  Plugin 'leafgarland/typescript-vim'

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
autocmd FocusGained,BufEnter * checktime

set tabstop=4 shiftwidth=4
set expandtab
set number
set ttimeout
set ttimeoutlen=100
set hidden
set laststatus=2
set wildmenu
set wildmode=longest:full,full
" TODO build & target is to generic?
set wildignore+=.git,*.orig,*/node_modules/*,*/build/*,*/target/*
set scrolloff=5
set undofile
set backspace=indent,eol,start
set incsearch
set hlsearch
set relativenumber
set completeopt=longest,menu,preview
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

" TODO Clean up
autocmd FileType typescript.tsx setlocal noexpandtab shiftwidth=2
autocmd FileType typescript setlocal noexpandtab shiftwidth=2
autocmd FileType typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType javascript.jsx setlocal noexpandtab shiftwidth=2
autocmd FileType javascript setlocal noexpandtab shiftwidth=2
autocmd FileType less setlocal noexpandtab shiftwidth=2
autocmd FileType scss setlocal noexpandtab shiftwidth=2
autocmd FileType sh setlocal expandtab tabstop=2 shiftwidth=2

" TODO Practice this
" map <leader>o :Files<CR>

" TODO Use :grep instead
" if executable('ag')
"   let $FZF_DEFAULT_COMMAND = 'ag -g ""'
"   let g:ackprg = 'ag --vimgrep --smart-case' 
" endif
" nnoremap <leader>s :Ack! ""<Left>

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

" This makes * stay on the word instead of jumping to the next one
nnoremap * *N
" Navigate prev/next quickfix errors
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]q :cnext<CR>
" Clear search higlight
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" TODO Ideally, should respect .gitignore (if available), or at least some
" default patterns (node_modules etc)

" Make grepping a bit more user friendly.
function! Grep(...)
  " The /dev/null at the end kicks in if a file argument is omitted
  " (i.e. will give an empty result instead of searching everywhere)
  return system('grep -n ' . expandcmd(join(a:000, ' ')) . ' /dev/null')
endfunction

command! -nargs=+ -complete=file_in_path Grep cgetexpr Grep(<f-args>)

" Open up the quickfix window when I do :Grep
augroup quickfix
  autocmd!
  autocmd QuickFixCmdPost cgetexpr cwindow
augroup END

" Abbreviation that changes :grep into :Grep
cnoreabbrev <expr> grep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'Grep' : 'grep'

" Clear hlsearch automatically
augroup vimrc-incsearch-highlight
  autocmd!
  autocmd CmdlineEnter [/\?] :set hlsearch
  autocmd CmdlineLeave [/\?] :set nohlsearch
augroup END
