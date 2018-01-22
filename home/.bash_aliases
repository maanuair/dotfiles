# -*- mode: shell-script ;-*-

alias c="clear"
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

alias j=jobs

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

# Special alias :-)
read -d '' BASH_REMINDERS_TXT<<"EOF"
Shortcuts:
  !!
    Repeats the last command typed, as is. So:
      $ echo foo1 bar2
      $ !!        # Produce the same output "foo1 bar2"
    While:
      $ echo foo1 bar2
      $ echo !!   # Produces the output "echo foo1 bar2"
    (note the echo prefix above)

  !$
    Repeats the last argument of the last command, e.g.:
      $ grep somestring /long/path/to/some/file/or/other.txt
      $ emacs !$

  ESC-.
   Interestingly enough, rpessing ESC followed by "." (dot key) repeats
   the last argument as well, and repeating ESC-. takes the former though
   history

  !*
    Takes all the arguments to the previous command and drops them in. So:
      $ grep isthere /long/path/to/some/file/or/other.txt
      $ egrep !*
      $ fgrep !*

  !:1-$
    Same than !*, but controls the arguments to repeat:
    - the ! means ‘look at the previous command’
    - the : is a separator
    - the 1 means ‘take the first word’
    - the - means ‘until’
    - the $ means ‘the last word’.
    So:
      $ grep isthere /long/path/to/some/file/or/other.txt
      $ egrep !:1-$
      $ fgrep !:1-$
    Or also:
      $ echo w1 w2 w3 && echo !:1-2 # will output w1 w2
      $ echo w1 w2 w3 && echo !:2-3 # will output w2 w3

  :h
    Put it after a filename, it will change that filename
    to remove everything up to the folder. Like this:
      $ grep isthere /long/path/to/some/file/or/other.txt
      $ cd !$:h

  `` vs $()
    Both forms substitute the output of the command contained
    within it into the command. $() is easier to read/write
    when nesting though..

  <(cmd)
    Executes the given cmd, and treat the output as a file. Like:
      $ diff <(grep somestring file1) <(grep somestring file2)

  [] or [[ ]] to test ?
     [ is the original form for tests, and then [[ was introduced
     to avoid treating the left operand as missing.
     Typically, don't write:
       if [ "x$(grep not_there /dev/null)" = "x" ]
     But instead:
       if [[ $(grep not_there /dev/null) = '' ]]
     Also:
     - word splitting and pathname expansion don't happen in [[...]];
     -  "==" does pattern matching in [[...]], but it does string comparison in [...].

  set -x
    Outputs the commands that get run as they run

  set -e
    Exits from a script if any command returned a non-zero exit code

  parameter expansion sub-string retrieval
    parameter     result
    -----------   ------------------------------
    $name         polish.ostrich.racing.champion
    ${name#*.}           ostrich.racing.champion
    ${name##*.}                         champion
    ${name%%.*}   polish
    ${name%.*}    polish.ostrich.racing
    ${name%.*.*}  polish.ostrich
    ${name#*.*.}                 racing.champion

  Bash FAQ
    Go to http://mywiki.wooledge.org/BashFAQ

  Bash inline help
    There is a help command!
      $ help test
EOF
alias bash_reminders='echo "$BASH_REMINDERS_TXT" | less'
