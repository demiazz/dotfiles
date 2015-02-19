if empty(glob('~/.nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.nvim/plugged')

  " Interface

    " Startup screen
    Plug 'mhinz/vim-startify'

    " Status line
    Plug 'itchyny/lightline.vim'

    " Solarized color scheme
    Plug 'altercation/vim-colors-solarized'

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

    " Trailing whitespaces
    Plug 'rondale-sc/vim-spacejam'

    " Pair parentheses, brackets, quotes, XML tags, and more.
    Plug 'tpope/vim-surround'

    " Indent highlight
    Plug 'Yggdroot/indentLine'

    " Git support
    Plug 'tpope/vim-fugitive'

    " Snippets
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'

  " Languages

    " Syntax checking
    Plug 'scrooloose/syntastic'

    " Ruby
    Plug 'vim-ruby/vim-ruby'
    Plug 'tpope/vim-endwise'

    " JavaScript
    Plug 'pangloss/vim-javascript',          { 'for': 'javascript' }
    Plug 'walm/jshint.vim',                  { 'for': 'javascript' }

    " CoffeeScript
    Plug 'kchmck/vim-coffee-script',         { 'for': 'coffee' }

    " JavaScript with React JSX
    Plug 'mxw/vim-jsx'

    " CoffeeScript with React JSX
    Plug 'mtscout6/vim-cjsx'

    " Erlang
    Plug 'jimenezrick/vimerl',               { 'for': 'erlang' }
    Plug 'edkolev/erlang-motions',           { 'for': 'erlang' }

    " Elixir
    Plug 'elixir-lang/vim-elixir',           { 'for': 'elixir' }

    " Scala
    Plug 'derekwyatt/vim-scala',             { 'for': 'scala' }
    Plug 'ktvoelker/sbt-vim',                { 'for': 'scala' }

    " Go
    Plug 'fatih/vim-go',                     { 'for': 'go' }

  " Frameworks

    " Ruby On Rails
    Plug 'tpope/vim-rails',                  { 'for': 'ruby' }

    " RSpec
    Plug 'thoughtbot/vim-rspec',             { 'for': 'ruby' }

    " node.js/io.js
    Plug 'moll/vim-node',                    { 'for': 'javascript' }

    " jQuery
    Plug 'itspriddle/vim-jquery',            { 'for': 'javascript' }

  " HTML/Templating

    " HTML
    Plug 'othree/html5.vim',                 { 'for': 'html' }
    Plug 'hokaccha/vim-html5validator',      { 'for': 'html' }
    Plug 'gregsexton/MatchTag',              { 'for': 'html' }

    " Slim
    Plug 'slim-template/vim-slim',           { 'for': 'slim' }

    " Haml
    Plug 'tpope/vim-haml',                   { 'for': ['haml', 'sass', 'scss'] }

    " Handlebars
    Plug 'mustache/vim-mustache-handlebars'

    " Mustache
    " Plug 'mustache/vim-mustache-handlebars'

    " Jade
    Plug 'digitaltoad/vim-jade',             { 'for': 'jade' }

    " Jinja2
    Plug 'Glench/Vim-Jinja2-Syntax',         { 'for': 'jinja' }

  " Styles

    " CSS
    Plug 'hail2u/vim-css3-syntax',           { 'for': 'css' }
    Plug 'ap/vim-css-color',                 { 'for': 'css' }
    Plug 'miripiruni/vim-better-css-indent', { 'for': 'css' }
    Plug 'gorodinskiy/vim-coloresque',       { 'for': 'css' }

    " SASS
    " Plug 'tpope/vim-haml'

    " SCSS
    " Plug 'tpope/vim-haml'

    " Less
    Plug 'groenewege/vim-less',              { 'for': 'less' }

    " Stylus
    Plug 'wavded/vim-stylus',                { 'for': 'stylus' }

  " Data formats

    " JSON
    Plug 'elzr/vim-json',                    { 'for': 'json' }

    " YAML
    Plug 'ingydotnet/yaml-vim',              { 'for': ['yaml', 'ansible'] }
    Plug 'chase/vim-ansible-yaml',           { 'for': 'ansible' }

  " User config

  Plug 'demiazz/vimrc'

call plug#end()
