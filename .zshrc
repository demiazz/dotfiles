#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Node

export NVM_DIR=$HOME/.nvm
source $(brew --prefix nvm)/nvm.sh

# Go

export GOPATH=$HOME/.go
export   PATH=$PATH:$GOPATH/bin

# Aliases

alias mux="chruby-exec system -- /usr/local/bin/mux"
