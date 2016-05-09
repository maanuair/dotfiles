# -*- mode: shell-script ;-*-
myInfo "Loading aliases..."

alias cp="cp -i"

alias df="df -h"
alias du="du -h"

alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

# Colorize ls: test the *BSD + Darwin "ls -G" color option vs GNU's one "ls --color=auto"
ls -G &> /dev/null &&\
  alias ls="ls -FG" &&\
  alias l="ls -aFG" &&\
  alias ll="ls -alhFG"
ls --color=auto &> /dev/null &&\
 alias ls="ls -F --color=auto" &&\
 alias l="ls -aF --color=auto" &&\
 alias ll="-alhF --color=auto"

alias mv="mv -i"

alias rm="rm -i"
