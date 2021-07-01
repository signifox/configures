source $HOME/.local/share/miniplug.zsh
miniplug plugin 'zsh-users/zsh-history-substring-search'
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug theme  'dracula/zsh'
miniplug load

#PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
#MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

stty -ixon
([[ -f $TMUX ]] && export TERM=screen-256color) || (export TERM=xterm-256color)

alias initmux='tmux -2u att'
alias restmux='tmux attach'
alias ff='fg %1'
alias gg='fg %2'

alias rm='ls -l'
#alias sbrm='/usr/local/opt/coreutils/libexec/gnubin/rm'
alias s='ls'
alias vi='vim'

export EDITOR=vim
export LESSCHARSET=utf-8
