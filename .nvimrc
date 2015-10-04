"==============================================================================
" Packages
"==============================================================================

if empty(glob('~/.nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.nvim/plugged')

  " Interface
    " Status line
    Plug 'itchyny/lightline.vim'

    " Color scheme
    Plug 'altercation/vim-colors-solarized'
    Plug 'chriskempson/vim-tomorrow-theme'

    " A tree explorer plugin
    Plug 'scrooloose/nerdtree'
    Plug 'jistr/vim-nerdtree-tabs'

    " Comments
    Plug 'tpope/vim-commentary'

    " Search
    Plug 'mileszs/ack.vim'
    Plug 'vim-scripts/IndexedSearch'

    " File open
    Plug 'kien/ctrlp.vim'
    Plug 'FelikZ/ctrlp-py-matcher'

    " Trailing whitespaces
    Plug 'rondale-sc/vim-spacejam'

    " Pair parentheses, brackets, quotes, XML tags, and more.
    Plug 'tpope/vim-surround'

    " Indent highlight
    Plug 'Yggdroot/indentLine'

    " Git support
    Plug 'tpope/vim-fugitive'

  " Languages

    " Syntax checking
    Plug 'scrooloose/syntastic'

    " Ruby
    Plug 'vim-ruby/vim-ruby'
    Plug 'tpope/vim-endwise'

    " JavaScript
    Plug 'pangloss/vim-javascript',                    { 'for': 'javascript' }
    Plug 'walm/jshint.vim',                            { 'for': 'javascript' }

    " CoffeeScript
    Plug 'kchmck/vim-coffee-script',                   { 'for': 'coffee' }

    " JavaScript with React JSX
    Plug 'mxw/vim-jsx'

    " CoffeeScript with React JSX
    Plug 'mtscout6/vim-cjsx'

    " Erlang
    Plug 'jimenezrick/vimerl',                         { 'for': 'erlang' }
    Plug 'edkolev/erlang-motions',                     { 'for': 'erlang' }

    " Elixir
    Plug 'elixir-lang/vim-elixir',                     { 'for': 'elixir' }

    " Scala
    Plug 'derekwyatt/vim-scala',                       { 'for': 'scala' }
    Plug 'ktvoelker/sbt-vim',                          { 'for': 'scala' }

    " Go
    Plug 'fatih/vim-go',                               { 'for': 'go' }

    " TypeScript
    Plug 'leafgarland/typescript-vim',                 { 'for': 'typescript' }

    " Rust
    Plug 'rust-lang/rust.vim',                         { 'for': 'rust' }

    " Haskell
    Plug 'raichoo/haskell-vim',                        { 'for': 'haskell' }

    " Ocaml
    Plug 'def-lkb/ocp-indent-vim',                     { 'for': 'ocaml' }

    " Clojure
    Plug 'guns/vim-clojure-static',                    { 'for': 'clojure' }
    Plug 'tpope/vim-fireplace',                        { 'for': 'clojure' }
    Plug 'vim-scripts/paredit.vim',                    { 'for': 'clojure' }
    Plug 'tpope/vim-repeat',                           { 'for': 'clojure' }
    Plug 'guns/vim-sexp',                              { 'for': 'clojure' }
    Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' }

  " Frameworks

    " Ruby On Rails
    Plug 'tpope/vim-rails',                            { 'for': 'ruby' }

    " RSpec
    Plug 'thoughtbot/vim-rspec',                       { 'for': 'ruby' }

    " node.js/io.js
    Plug 'moll/vim-node',                              { 'for': 'javascript' }

    " jQuery
    Plug 'itspriddle/vim-jquery',                      { 'for': 'javascript' }

  " HTML/Templating

    " HTML
    Plug 'othree/html5.vim',                           { 'for': 'html' }
    Plug 'hokaccha/vim-html5validator',                { 'for': 'html' }
    Plug 'gregsexton/MatchTag',                        { 'for': 'html' }

    " Slim
    Plug 'slim-template/vim-slim',                     { 'for': 'slim' }

    " Haml
    Plug 'tpope/vim-haml',                             { 'for': ['haml', 'sass', 'scss'] }

    " Handlebars
    Plug 'mustache/vim-mustache-handlebars'

    " Mustache
    " Plug 'mustache/vim-mustache-handlebars'

    " Jade
    Plug 'digitaltoad/vim-jade',                       { 'for': 'jade' }

    " Jinja2
    Plug 'Glench/Vim-Jinja2-Syntax',                   { 'for': 'jinja' }

    " Markdown
    Plug 'godlygeek/tabular',                          { 'for': 'markdown' }
    Plug 'plasticboy/vim-markdown',                    { 'for': 'markdown' }

  " Styles

    " CSS
    Plug 'hail2u/vim-css3-syntax',                     { 'for': 'css' }
    Plug 'ap/vim-css-color',                           { 'for': 'css' }
    Plug 'miripiruni/vim-better-css-indent',           { 'for': 'css' }
    Plug 'gorodinskiy/vim-coloresque',                 { 'for': 'css' }

    " SASS
    " Plug 'tpope/vim-haml'

    " SCSS
    " Plug 'tpope/vim-haml'

    " Less
    Plug 'groenewege/vim-less',                        { 'for': 'less' }

    " Stylus
    Plug 'wavded/vim-stylus',                          { 'for': 'stylus' }

  " Data formats

    " JSON
    Plug 'elzr/vim-json',                              { 'for': 'json' }

    " YAML
    Plug 'ingydotnet/yaml-vim',                        { 'for': ['yaml', 'ansible'] }
    Plug 'chase/vim-ansible-yaml',                     { 'for': 'ansible' }

call plug#end()

" Ocaml

let output = system('which opam')
if !v:shell_error
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute "set rtp+=" . g:opamshare . "/merlin/vim"
endif

"==============================================================================
" Environment
"==============================================================================

" Compatibility (only for Vim, nvim ignore this option)

  set nocompatible

" History

  set history=1000

" Backspace

  set backspace=indent,eol,start

" Backup and swp files

  " Don't backup
  set nobackup

  " Don't create swap files
  set noswapfile

"==============================================================================
" Interface
"==============================================================================

" Encoding and file format

  " Encoding inside vim
  if !has('nvim')
    set encoding=utf-8
  endif

  " Default file encoding
  set fileencodings=utf-8,cp1251

  " File format
  set fileformat=unix

" Wildmenu

  " Enable wildmenu
  set wildmenu

  " Wildmenu completion mode
  set wildmode=list:longest,full

  " Ignore list for wildmenu

    " Version control
    set wildignore+=.hg,.git,.svn

    " OS X
    set wildignore+=*.DS_Store

    " Python byte code
    set wildignore+=*.pyc

    " Binary images
    set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg,*.psd


" Linebreak

  " Wrap long lines
  set wrap

  " Don't break words when wrapping
  set linebreak

  " Show ↪ at the beginning of wrapped lines
  if has("linebreak")
    let &sbr = nr2char(8618).' '
  endif

  set nolist

  " Number of column to be highlighted
  set colorcolumn=80

  " Maximum width of text that is being inserted
  " Longer lines will be broken after white space to get this width
  set textwidth=0
  set wrapmargin=0

  set formatoptions+=l

  highlight ColorColumn ctermbg=187


" Indent

  " Copy indent from current line when starting a new line
  set autoindent

  " Do smart indenting when starting a new line
  set smartindent

  " Number of spaces to use for each step of (auto)indent
  set shiftwidth=2

  " Use spaces instead of tab
  set expandtab

  " Number of spaces that a tab counts for
  set tabstop=2

  " Number of spaces that a tab counts for while performing editing operations
  set softtabstop=2


" Cursorline and cursorcolumn

  " Enable cursorline only for active window
  augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
  augroup END

" Visualbell

  " Disable any beeps and flashes
  set visualbell
  set t_vb=

" Line numbers

  set relativenumber

" Autoread

  " Auto read when a file is changed from the outside
  set autoread

" Lines to the cursor

  set so=7

" Status line

  " Always show statusline
  set laststatus=2

" Search paths

  " Relative to the directory of the current file
  " Current directory
  " Downwards in a directory tree
  set path=.,,**

" Tabs

  " Always show tabs
  set showtabline=2

" Search

  " Incremental search
  set incsearch

  " Highlight search
  set hlsearch

  " Ignore case in search
  set ignorecase

  " Smart search case
  set smartcase

  set gdefault

" Mapping

  " Map russian cyrillic keys
  set langmap=ёйцукенгшщзхъфывапролджэячсмитьбюЁЙЦУКЕHГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ;`qwertyuiop[]asdfghjkl\\;'zxcvbnm\\,.~QWERTYUIOP{}ASDFGHJKL:\\"ZXCVBNM<>


" GUI

if has("gui_running")
  set guifont=Fira\ Mono:h14
  set lines=999 columns=999
endif

"==============================================================================
" Mappings
"==============================================================================

" Leader key

  let mapleader = ","

" Clear search results in normal mode

  nnoremap <silent> <Esc><Esc> :nohlsearch<CR><Esc>

" Switch splits

  nmap <C-h> <C-W>h
  nmap <C-j> <C-W>j
  nmap <C-k> <C-W>k
  nmap <C-l> <C-W>l

" Close split

  nmap <Leader>qq :close<CR>

" Relative windows

  nmap <Leader><Left>  :leftabove vnew<CR>
  nmap <Leader><Right> :rightbelow vnew<CR>
  nmap <Leader><Up>    :leftabove new<CR>
  nmap <Leader><Down>  :rightbelow new<CR>

" Buffers

  nmap <Leader>bl :ls<CR>:b
  nmap <Leader>bp :bp<CR>
  nmap <Leader>bn :bn<CR>

" Tabs

  nmap <Tab>   gt
  nmap <S-Tab> gT

" Disable arrow keys

  inoremap <Up>    <NOP>
  inoremap <Down>  <NOP>
  inoremap <Left>  <NOP>
  inoremap <Right> <NOP>

  noremap  <Up>    <NOP>
  noremap  <Down>  <NOP>
  noremap  <Left>  <NOP>
  noremap  <Right> <NOP>

" Ctrl + s

  map <C-s>  <Esc>:w<CR>
  imap <C-s> <Esc>:w<CR>

" Number lines mode switch

  " http://stackoverflow.com/questions/4387210/vim-how-to-map-two-tasks-under-one-shortcut-key
  " Vim 7.3 required
  let g:relativenumber = 1

  function! ToogleRelativeNumber()
    if g:relativenumber == 0
      let g:relativenumber = 1
      set nonumber
      set relativenumber
      echo "Show relative line numbers"
    else
      let g:relativenumber = 0
      set norelativenumber
      set number
      echo "Show line numbers"
    endif
  endfunction

  map <Leader>nm :call ToogleRelativeNumber()<CR>

"==============================================================================
" Syntax
"==============================================================================

au BufNewFile,BufRead *.markdown,*.mdown,*.mkd,*.mkdn,*.mdwn,*.md  set ft=markdown

"==============================================================================
" Vendor
"==============================================================================

" NERDTree

  let NERDTreeShowBookmarks=1
  let NERDTreeChDirMode=2
  let NERDTreeQuitOnOpen=1
  let NERDTreeShowHidden=1
  let NERDTreeKeepTreeInNewTab=0
  let NERDTreeMinimalUI=1
  let NERDTreeDirArrows=1
  let NERDTreeBookmarksFile= $HOME . '/.nvim/.NERDTreeBookmarks'

  nmap <Bs> :NERDTreeTabsToggle<CR>

" Tomorrow

  " Number of colors
  set t_Co=256

  " Use light background
  set background=dark

  " Enable Tomorrow colorscheme
  colorscheme Tomorrow-Night-Bright

  " Enable syntax highlight
  syntax enable

" Lightline

  let g:lightline = {
        \   'colorscheme': 'Tomorrow_Night_Bright',
        \   'active': {
        \   'left': [ [ 'mode', 'paste' ],
        \           [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
        \   },
        \   'component': {
        \     'readonly': '%{&filetype=="help"?"":&readonly?"⭤":""}',
        \     'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
        \     'fugitive': '%{exists("*fugitive#statusline")?fugitive#statusline():""}'
        \   },
        \   'component_visible_condition': {
        \     'readonly': '(&filetype!="help"&& &readonly)',
        \     'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
        \     'fugitive': '(exists("*fugitive#statusline") && ""!=fugitive#statusline())'
        \   }
        \ }

" IndentLine

  let g:indentLine_color_term = 187
  let g:indentLine_char       = '¦'

" vim-json

  let g:vim_json_syntax_conceal = 0

" ctrlp

let g:ctrlp_user_command = 'ag %s -i --nocolor
                                   \ --nogroup
                                   \ --hidden
                                   \ --ignore .git
                                   \ --ignore .svn
                                   \ --ignore .hg
                                   \ --ignore .DS_Store
                                   \ --ignore "node_modules/**/*"
                                   \ -g ""'

let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
