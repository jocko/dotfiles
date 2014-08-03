call pathogen#infect()
call pathogen#helptags()

set tabstop=2 shiftwidth=2
set expandtab

let mapleader=" "

set number

" Show invisibles
set list
set listchars=tab:▸\ ,eol:¬

autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

set incsearch
set ignorecase
set smartcase

" Theme
if !has("gui_running")
  let g:gruvbox_italic=0
endif
colorscheme gruvbox
set background=dark

" Neocomplete
let g:neocomplete#enable_at_startup = 1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
