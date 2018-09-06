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
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
Plug 'alpertuna/vim-header'
Plug 'skielbasa/vim-material-monokai'
Plug 'justmao945/vim-clang'
Plug 'easymotion/vim-easymotion'
Plug 'bling/vim-bufferline'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'jceb/vim-orgmode'
Plug 'rust-lang/rust.vim'
Plug 'dracula/vim'
Plug 'sjl/gundo.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'jremmen/vim-ripgrep'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !has("gui_running")
    set t_Co=256
    set term=screen-256color
endif
set background=light
colorscheme dracula


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1
set laststatus=2
let g:airline_powerline_fonts = 0
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

if executable('rg')
  set grepprg=rg\ --color=never
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-clang
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clang_c_options = '-std=gnu11'
let g:clang_cpp_options = '-std=c++11 -stdlib=libc++'

let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1


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
"Lsp
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Rust
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:rustfmt_autosave = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"gundo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gundo_width = 60
let g:gundo_preview_height = 40
let g:gundo_right = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"multi-cursors
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 多光标操作
let g:multi_cursor_use_default_mapping=0
" Default mapping
" ctrl+m 选中一个
" ctrl+p 放弃一个, 回到上一个
" ctrl+x 跳过当前选中, 选中下一个
" esc    退出
let g:multi_cursor_next_key='<C-m>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Keybind
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <F5>  :GundoToggle<cr>
nmap <silent> <F7>  :TagbarToggle<cr>
nmap <silent> <F8>  :NERDTreeToggle<cr>
nmap <silent> <F11> :bp!<cr>
nmap <silent> <F12> :bn!<cr>

