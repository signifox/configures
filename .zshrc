if [[ ! -f $HOME/.local/share/miniplug.zsh ]]; then
curl \
  -sL --create-dirs \
  https://git.sr.ht/~yerinalexey/miniplug/blob/master/miniplug.zsh \
  -o $HOME/.local/share/miniplug.zsh
fi

source $HOME/.local/share/miniplug.zsh
miniplug plugin 'zsh-users/zsh-history-substring-search'
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug theme  'dracula/zsh'
miniplug load

#PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
#MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

([[ -f $TMUX ]] && export TERM=screen-256color) || (export TERM=xterm-256color)

alias initmux='tmux -2u att'
alias restmux='tmux attach'
alias ff='fg %1'
alias gg='fg %2'

alias rm='ls -l'
#alias sbrm='/usr/local/opt/coreutils/libexec/gnubin/rm'
alias s='ls'
alias vi='vim'

export EDITOR=nvim
export LESSCHARSET=utf-8
export HOMEBREW_NO_AUTO_UPDATE=true
export PATH=$HOME/.cargo/bin:$PATH

