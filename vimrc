call pathogen#infect()
call pathogen#helptags()

set tabstop=2 shiftwidth=2
set expandtab 
 
set number

set hlsearch 
set incsearch
set ignorecase
set smartcase 

if !has("gui_running")
  let g:gruvbox_italic=0
endif
colorscheme gruvbox
set background=dark
