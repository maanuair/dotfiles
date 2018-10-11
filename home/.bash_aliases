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

alias ncdu="ncdu --color dark"  # -rr to prevent deletion and spawning shell

alias rm="rm -iv"

alias t="todo.sh"

# Special alias :-)
read -d '' REMIND_BASH_TXT<<"EOF"
Bash shortcuts
==============
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
   Interestingly enough, pressing ESC followed by "." (dot key) repeats
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
    when nesting though.

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

Bash interesting commands
=========================

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

  help
    There is a builtin help for builtin command!
      $ help cd

Bash FAQ
========

  Go to http://mywiki.wooledge.org/BashFAQ
EOF
alias remind-bash='echo "$REMIND_BASH_TXT" | less'

read -d '' REMIND_BREW_TXT<<"EOF"
Brew interesting packages:
=========================

  Interesting brew packages:
    atomicparsley  - MPEG-4 command-line tool
    bat            - A better cat with syntax highlighting
    bitwarden-cli  - Secure and free password manager for all of your devices
    diff-so-fancy  - Good-lookin' diffs with diff-highlight and more
    fd             - Simple, fast and user-friendly alternative to find
    git            - Yo know git, do you?
    graphicsmagick - Image processing tools collection
    htop           - Improved top
    imagemagick    - Tools and libraries to manipulate images in many formats
    markdown       - Text-to-HTML conversion tool
    mps-youtube    - Terminal based YouTube player and downloader
    ncdu           - NCurses Disk Usage
    sox            - SOund eXchange: universal sound sample translator
    tldr           - Simplified and community-driven man pages
    todo-txt       - Minimal, todo.txt-focused editor
    youtube-dl     - Download YouTube videos from the command-line

Brew funny packages:
===================
    figlet         - Banner-like program prints strings as ASCII art (c.f. http://www.figlet.org/examples.html)
    lolcat         - Rainbows and unicorns in your console!


Brew intesting casks:
====================

  betterzip        - Most notably has a Quick Look for zip file
  freeplane        - A mindmapper
  gimp             - The GNU Image Manipulation Program
  qlcolorcode      - Quick Look plugin that renders source code with syntax highlighting
  qlmarkdown       - Quick Look plugin that renders markdown files
  qlstephen        - QuickLook plugin that renders plain text files without a file extension
  quicklook-json   - Quick Look plugin that renders json files
  quicklookapk     - Quick Look plugin that renders APK files

Brew interesting commands:
========================

  brew cleanup -s [--dry-run]
    Delete the brew cache

  brew update && brew upgrade && brew cleanup
    Update the brew packages, and remove their cache

EOF
alias remind-brew='echo "$REMIND_BREW_TXT" | less'
