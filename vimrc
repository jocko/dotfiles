set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
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

  " Git wrapper
  Plugin 'tpope/vim-fugitive'
  " Reopen files at last edit position
  Plugin 'farmergreg/vim-lastplace'
  " Automatically add end statements, e.g. in Ruby, end after def etc.
  Plugin 'tpope/vim-endwise.git'

  " Themes
  Plugin 'morhetz/gruvbox'
  Plugin 'liuchengxu/space-vim-dark'
  Plugin 'arcticicestudio/nord-vim'

  Plugin 'vim-ruby/vim-ruby'

  Plugin 'pangloss/vim-javascript'
  Plugin 'leafgarland/typescript-vim'

  Plugin 'maxmellon/vim-jsx-pretty'
  " Plugin 'prettier/vim-prettier'

  " TODO Can I get rid of this?
  Plugin 'tpope/vim-vinegar'
  " TODO What are the 'native' alternatives to this?
  Plugin 'mileszs/ack.vim'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-surround'
  Plugin 'aquach/vim-http-client'
  Plugin 'junegunn/fzf'
  Plugin 'junegunn/fzf.vim'
  Plugin 'jpalardy/vim-slime'
  Plugin 'dense-analysis/ale'

  call vundle#end()

  color nord
endif

filetype plugin indent on
syntax enable

" Makes :fin[d]  look for files recursively
" TODO Should i remove the /usr/include default entry?
set path+=**

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

" TODO Test this out
" Taken from https://blog.ffff.lt/posts/typescript-and-ale/
" autocmd FileType javascript map <buffer> <c-]> :ALEGoToDefinition<CR>
" autocmd FileType typescript map <buffer> <c-]> :ALEGoToDefinition<CR>
" autocmd FileType typescriptreact map <buffer> <c-]> :ALEGoToDefinition<CR>
" let js_fixers = ['prettier', 'eslint']

" let g:ale_fixers = {
" \   '*': ['remove_trailing_lines', 'trim_whitespace'],
" \   'javascript': js_fixers,
" \   'javascript.jsx': js_fixers,
" \   'typescript': js_fixers,
" \   'typescriptreact': js_fixers,
" \   'css': ['prettier'],
" \   'json': ['prettier'],
" \}
