filetype off

set rtp+=~/.nvim/bundle/Vundle.vim
call vundle#begin('~/.nvim/bundle')

Plugin 'gmarik/Vundle.vim'

" Interface

  " Solarized color scheme
  Plugin 'altercation/vim-colors-solarized'

  " A tree explorer plugin
  Plugin 'scrooloose/nerdtree'
  Plugin 'jistr/vim-nerdtree-tabs'

" Languages

  " Ruby
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'tpope/vim-endwise'

  " JavaScript
  Plugin 'pangloss/vim-javascript'
  Plugin 'walm/jshint.vim'

  " CoffeeScript
  Plugin 'kchmck/vim-coffee-script'

" Frameworks

  " Ruby On Rails
  Plugin 'tpope/vim-rails'

  " node.js/io.js
  Plugin 'moll/vim-node'

  " jQuery
  Plugin 'itspriddle/vim-jquery'

" HTML/Templating 

  " HTML
  Plugin 'othree/html5.vim'
  Plugin 'hokaccha/vim-html5validator'
  Plugin 'gregsexton/MatchTag'

  " Slim
  Plugin 'slim-template/vim-slim'

  " Haml
  Plugin 'tpope/vim-haml'

  " Handlebars
  Plugin 'mustache/vim-mustache-handlebars'

  " Mustache
  " Plugin 'mustache/vim-mustache-handlebars'

  " Jade
  Plugin 'digitaltoad/vim-jade'

  " Jinja2
  Plugin 'Glench/Vim-Jinja2-Syntax'

" Styles

  " CSS
  Plugin 'hail2u/vim-css3-syntax'
  Plugin 'ap/vim-css-color'
  Plugin 'miripiruni/vim-better-css-indent'
  Plugin 'gorodinskiy/vim-coloresque'

  " SASS
  " Plugin 'tpope/vim-haml'

  " SCSS
  " Plugin 'tpope/vim-haml'

  " Less
  Plugin 'groenewege/vim-less'

  " Stylus
  Plugin 'wavded/vim-stylus'

" Data formats

  " JSON
  Plugin 'elzr/vim-json'

  " YAML
  Plugin 'ingydotnet/yaml-vim'
  Plugin 'chase/vim-ansible-yaml'

call vundle#end()
filetype plugin indent on
