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

set guifont=Hack\ Bold:h14

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
" Auto append vim setting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufNewFile *.h,*.cpp,*.c,*.cc,*.java,*.pl,*.php
\    :call append(line('$'), "/* vim: set ts=4 sw=4 sts=4 tw=100 */")



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Plugin Setting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'w0rp/ale'
Plug 'tell-k/vim-autopep8'
Plug 'rhysd/vim-clang-format'
Plug 'kien/ctrlp.vim'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'fholgado/minibufexpl.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
Plug 'alpertuna/vim-header'
Plug 'whatot/gtags-cscope.vim'
Plug 'skielbasa/vim-material-monokai'



call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256
let g:solarized_termcolors=256
"set background=dark
set background=light
"set termguicolors
colorscheme material-monokai


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1
set laststatus=2
let g:airline_powerline_fonts = 0
"let g:airline_theme='molokai'
let g:airline_theme='materialmonokai'
let g:materialmonokai_subtle_airline=1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"MiniBufExplorer
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1
let g:miniBufExplMoreThanOne=0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"nerdtree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDTreeShowLineNumbers=1
let NERDTreeAutoCenter=1
let NERDTreeShowHidden=1
let NERDTreeWinSize=31
let NERDTreeIgnore=['\.pyc','\~$','\.swp','\.o']
let NERDTreeShowBookmarks=1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"ctrlp
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
if has("win32") || has("win64")
    let g:ctrlp_user_command = 'dir %s /-n /b /s /a-d'  " Windows
    set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
elseif has("unix")
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " Linux/MacOSX
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-autopep8
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:autopep8_aggressive=1
let g:autopep8_max_line_length=256
let g:autopep8_ignore="E123,E133,E501"
let g:autopep8_disable_show_diff=1
let g:flake8_show_in_gutter=1  " show
"autocmd BufWritePost *.py Autopep8


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
"vim-header
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:header_field_author = 'huxiao'
let g:header_field_author_email = 'huxiao@bytedance.com'
let g:header_field_timestamp_format = '%Y/%m/%d'
let g:header_auto_add_header = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"clang-format
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clang_format#command="/usr/bin/clang-format-3.5"
let g:clang_format#auto_format = 0
let g:clang_format#style_options = {
    \ "BasedOnStyle" : "Google"    ,
    \ "IndentWidth"  : 2           ,
    \ "TabWidth"     : 2           }


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Keybind
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <F7>  :TagbarToggle<cr> 
nmap <silent> <F8>  :NERDTreeToggle<cr> 
nmap <silent> <F11> :bp!<cr>
nmap <silent> <F12> :bn!<cr>

