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

Plug 'w0rp/ale'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-bufferline'
Plug 'sjl/gundo.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tell-k/vim-autopep8'
Plug 'rhysd/vim-clang-format'
Plug 'airblade/vim-gitgutter'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
Plug 'alpertuna/vim-header'
Plug 'justmao945/vim-clang'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'easymotion/vim-easymotion'
Plug 'dracula/vim'
Plug 'aceofall/gtags.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 't9md/vim-choosewin'
Plug 'simeji/winresizer'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !has("gui_running")
    set t_Co=256
    set term=screen-256color
endif
set background=dark
colorscheme dracula


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='dracula'
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'


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
let g:clang_format#detect_style_file = 1
let g:clang_format#enable_fallback_style=1
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

"cscope
set cscopetag                  " 使用 cscope 作为 tags 命令
set cscopeprg='gtags-cscope'   " 使用 gtags-cscope 代替 cscope

"gtags
let GtagsCscope_Auto_Load = 1
let CtagsCscope_Auto_Map = 1
let GtagsCscope_Quiet = 1


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
"vim-choosewin
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" invoke with '-'
nmap  -  <Plug>(choosewin)
" if you want to use overlay feature
let g:choosewin_overlay_enable = 1



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Keybind
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <F5>  :ClangFormat<cr>
nmap <silent> <F6>  :GundoToggle<cr>
nmap <silent> <F7>  :TagbarToggle<cr>
nmap <silent> <F8>  :NERDTreeToggle<cr>
nmap <silent> <F11> :bp!<cr>
nmap <silent> <F12> :bn!<cr>

"" ctrl+]将跳到光标所在变量或函数的定义处, ctrl+t返回
"" cs find s ---- 查找C语言符号，即查找函数名、宏、枚举值等出现的地方
"" cs find g ---- 查找函数、宏、枚举等定义的位置，类似ctags所提供的功能
"" cs find c ---- 查找调用本函数的函数
"" cs find t ---- 查找指定的字符串
"" cs find e ---- 查找egrep模式，相当于egrep功能
"" cs find d ---- 查找本函数调用的函数
"" cs find f ---- 查找并打开文件，类似vim的find功能
"" cs find i ---- 查找包含本文件的文件
nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-\>i :cs find i <C-R>=expand("<cfile>")<CR><CR>

