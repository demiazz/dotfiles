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

" Templating

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

  " SASS
  " Plugin 'tpope/vim-haml'

  " SCSS
  " Plugin 'tpope/vim-haml'

  " Less
  Plugin 'groenewege/vim-less'

  " Stylus
  Plugin 'wavded/vim-stylus'

call vundle#end()
filetype plugin indent on
