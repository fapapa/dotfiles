# Start configuration added by Zim install {{{
#
# User configuration sourced by all invocations of the shell
#

# Define Zim location
: ${ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim}
# }}} End configuration added by Zim install

export PATH=$HOME/bin:$HOME/.emacs.d/bin:/usr/local/bin:/usr/local/opt/postgresql@10/bin:/usr/local/opt/node@10/bin:/usr/local/opt/sqlite/bin:$PATH

# Bring in config that is sensitive and should not be in source control
. ~/.sensitive

export EDITOR='emacsclient --tty'
export ALTERNATE_EDITOR=''
export VISUAL='emacsclient --tty'

export PATH="/opt/local/bin:/opt/local/sbin:/usr/local/opt/qt@5.5/bin:/Library/TeX/texbin:$PATH"
export PATH="$PATH:/usr/local/smlnj/bin"
export PATH=$(brew --prefix grep)/libexec/gnubin:$PATH

eval "$($(brew --prefix)/bin/brew shellenv)"
export PATH="$(brew --prefix)/bin:$PATH"
export NVM_DIR="$HOME/.nvm"; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export AWS_PROFILE='autoawsume-default'
alias awsume='. awsume'

# Load rbenv
eval "$(rbenv init -)"

# Load nodenv
eval "$(nodenv init -)"

alias r=bin/rails
