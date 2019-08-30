export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="gentoo"
CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

stty -ixon
([[ -f $TMUX ]] && export TERM=screen-256color) || (export TERM=xterm-256color)

alias initmux='tmux -2u att'
alias restmux='tmux attach'
alias ff='fg %1'
alias gg='fg %2'
alias rm='ls -l'
alias sbrm='/usr/local/opt/coreutils/libexec/gnubin/rm'
alias s='ls'
alias vi='vim'

export EDITOR=vim
export LESSCHARSET=utf-8
export PATH=/usr/local/bin:$PATH
export ARCHFLAGS="-arch x86_64"
