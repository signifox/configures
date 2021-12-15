#!/usr/bin/env bash

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

brew install coreutils vim zsh tmux clang-format ripgrep

#install miniplug
curl \
  -sL --create-dirs \
  https://git.sr.ht/~yerinalexey/miniplug/blob/master/miniplug.zsh \
  -o $HOME/.local/share/miniplug.zsh

# Add to zshrc:
source "$HOME/.local/share/miniplug.zsh"

###########
#configure
##########

#https://github.com/signifox/configures/blob/master/vim/.vim/config.vim
#https://github.com/signifox/configures/blob/master/.zshrc
#https://github.com/signifox/configures/blob/master/.tmux.conf
#https://github.com/signifox/configures/blob/master/.gitconfig

############
#software
###########

#https://www.cakebrew.com/
#https://www.iterm2.com/
#https://code.visualstudio.com/
#https://sequelpro.com/test-builds
#https://freemacsoft.net/appcleaner/

