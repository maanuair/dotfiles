# -*- mode: shell-script ;-*-

alias cp="cp -i"

alias df="df -h"
alias du="du -h"

alias egrep="egrep --color=auto"

# Emacs specific alias on OSX
if [[ "$(uname -s)" == "Darwin" ]]
then
    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
fi

alias fgrep="fgrep --color=auto"

alias grep="grep --color=auto"

# OS dependent aliases for colorized ls. Test the *BSD + Darwin "ls -G" color option vs GNU's one "ls --color=auto"
if [[ "$(uname -s)" == "Darwin" ]]
then
    alias ls="ls -FG"
    alias l="ls -aFG"
    alias ll="ls -alhFG"
else
    alias ls="ls -F --color=auto"
    alias l="ls -aF --color=auto"
    alias ll="-alhF --color=auto"
fi

alias mv="mv -i"

alias rm="rm -i"
