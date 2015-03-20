# -*- mode: shell-script ;-*-

# Confirm interactive operations
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

# Default to human readable figures
alias df="df -h"
alias du="du -h"

# Misc :)
alias grep="grep --color"			# show differences in colour
alias egrep="egrep --color=auto"	# show differences in colour
alias fgrep="fgrep --color=auto"	# show differences in colour

# Some shortcuts for different directory listings
alias ls="ls -Fh" 			# classify, human readable
alias  l="ls -a"			# all
alias ll="l -l"				# long list
