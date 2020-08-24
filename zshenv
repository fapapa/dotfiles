export PATH=$HOME/bin:$HOME/.emacs.d/bin:/usr/local/bin:/usr/local/opt/postgresql@10/bin:/usr/local/opt/node@10/bin:/usr/local/opt/sqlite/bin:$PATH

# Bring in config that is sensitive and should not be in source control
. ~/.sensitive
export ZSH="/Users/fabiopapa/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh

export EDITOR='emacsclient -t "$@"'
export ALTERNATE_EDITOR=''
export VISUAL='emacsclient -nc "$@"'

export PATH="/opt/local/bin:/opt/local/sbin:/usr/local/opt/qt@5.5/bin:/Library/TeX/texbin:$PATH"

# Load rbenv
eval "$(rbenv init -)"

# Load nodenv
eval "$(nodenv init -)"
