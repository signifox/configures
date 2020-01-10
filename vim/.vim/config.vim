"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Vim Configuration
"Author: signifox
"Email:  signifox@gmail.com
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Font encode
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set termencoding=utf-8
set encoding=utf-8


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Global setting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
set expandtab
set smarttab
set cindent
set autoindent
set smartindent
set showmatch
set number
set ruler
set cursorline
set cursorcolumn
set nocp
set wildmenu
set hid
set smartcase
set hlsearch
set incsearch
set lazyredraw
set magic
set showmatch
set nowb
set nobackup
set noswapfile
set lbr
set wrap
set autowrite
set noeb
set novisualbell
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

set backspace=2
set tabstop=4
set softtabstop=4
set shiftwidth=4

set guifont=Hack:h14

syntax on
filetype plugin indent on
let mapleader = "'"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Vim autocmd
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType makefile set noexpandtab
autocmd FileType diff nmap <C-n> /^+\\|^-<CR>
autocmd FileType xml,html,c,cs,java,perl,shell,bash,cpp,python,vim,php,ruby set number


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Restore the last quit position when open file.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\     exe "normal g'\"" |
\ endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Plugin Setting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'w0rp/ale'
Plug 'majutsushi/tagbar'
Plug 'bling/vim-bufferline'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'google/vim-maktaba'
Plug 'google/vim-glaive'
Plug 'google/vim-codefmt'
Plug 'easymotion/vim-easymotion'
Plug 't9md/vim-choosewin'
Plug 'tomasr/molokai'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !has("gui_running")
    set t_Co=256
    set term=screen-256color
endif
set background=dark
colorscheme molokai

let g:molokai_original = 1
let g:rehash256 = 1
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='molokai'
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-ale
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_sign_column_always = 1
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"clang-format
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clang_format#command="/usr/local/bin/clang-format"
let g:clang_format#detect_style_file = 1
let g:clang_format#enable_fallback_style=1
let g:clang_format#auto_format = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-easymotion
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map f <Plug>(easymotion-prefix)
map ff <Plug>(easymotion-s)
map fs <Plug>(easymotion-f)
map fl <Plug>(easymotion-lineforward)
map fj <Plug>(easymotion-j)
map fk <Plug>(easymotion-k)
map fh <Plug>(easymotion-linebackward)
let g:EasyMotion_smartcase = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-choosewin
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" invoke with '-'
nmap  -  <Plug>(choosewin)
" if you want to use overlay feature
let g:choosewin_overlay_enable = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Keybind
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <F5>  :FormatCode<cr>
nmap <silent> <F7>  :TagbarToggle<cr>
nmap <silent> <F11> :bp!<cr>
nmap <silent> <F12> :bn!<cr>

