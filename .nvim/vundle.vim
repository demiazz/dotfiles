filetype off

set rtp+=~/.nvim/bundle/Vundle.vim
call vundle#begin('~/.nvim/bundle')

Plugin 'gmarik/Vundle.vim'

" Interface

  " Solarized color scheme
  Plugin 'altercation/vim-colors-solarized'
  Plugin 'gorodinskiy/vim-coloresque'

  " A tree explorer plugin
  Plugin 'scrooloose/nerdtree'
  Plugin 'jistr/vim-nerdtree-tabs'

" Styles

  " Stylus
  Plugin 'wavded/vim-stylus'

call vundle#end()
filetype plugin indent on
