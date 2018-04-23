#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/bin"

alias ls='ls --color=auto'
alias mplayer='mplayer -ao pulse'
alias top='htop'

#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh

GET_PROMPT() {
    export PS1=$($HOME/.bash-prompt)
}

PROMPT_COMMAND="GET_PROMPT"

TERM=xterm-256color
