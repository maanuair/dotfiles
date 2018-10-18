# My bash profile, normally hosted at https://github.com/maanuair/dotfiles

# Some log helpers
function myOut () {
  # A litle bit of blue
  printf "%s [\033[34mDOT-INFOR\033[0m] ${@}\033[0m\n" `date "+%H:%M:%S"`
}

function myErr () {
  # A little bit of red, redirected to stderr
  printf "%s [\033[31mDOT-ERROR\033[0m] ${@}\033[0m\n" `date "+%H:%M:%S"` 1>&2
}

function myDebug () {
  # And a little bit of highlighted green
  printf "%s [\033[30m\033[42mDOT-DEBUG\033[0m] ${@}\033[0m\n" `date "+%H:%M:%S"`
}

# Echo OS detected: osx, linux or windows (cygwin, MinGW...)
function getOS() {
  case "$(uname -s)" in
    Darwin)
      echo 'osx';;
    Linux)
      echo 'linux';;
    CYGWIN*)
      echo 'cygwin';;
    *) # Including MINGW32*|MSYS*...
      echo "$(uname -s) Unsupported !" 1>&2 ;;
  esac
}

# Ask the given $1 question, and read choice until it matches a choice in given $2 array.
# The empty response (i.e: Enter) would match $3 choice if given, otherwise it asks again.
function ask () {
  # Inputs check
  if [[ $# != 2 && $# != 3 ]]
  then
    myErr "Function $FUNCNAME(): bad arguments count, ignored."
    myErr "Synopsis: $FUNCNAME \"So we quit?\" \"[y|yes|Y|n|no|N]\" [\"N\"]"
    return;
  else
    # Ask!
    local REGEXP="^"`echo $2 | tr '[]' '()'`"$"
    local response
    while true; do
      read -r -p "$1 $2 " response
      if [[ "$response" == '' ]]; then
        echo $3
        break
      elif [[ "$response" =~ $REGEXP ]]; then
        echo $response
        break
      else
        # We loop until an expected response is given
        :
      fi
    done
  fi
}

# Turn on native strict symbolic links on Cygwin
function setupCygwin () {
  myOut "Setting up Cygwin..."

  # We want to use Windows native symbolink links on Cygwin!
  export CYGWIN="winsymlinks:nativestrict ${CYGWIN}"

  # But we must be Windows Administrator to make them work...
  id -G | grep -qE '\<(544|0)\>'; # Group 544 or 0 does mean Windows Administrator
  if [[ $? != 0 ]]; then
    myErr "You must run Cygwin as Administrator for native symbolic links available."
    if [[ $(ask "Continue anyway?" "[y|N]" N) == N ]]; then
      exit 1;
    fi;
  fi
}

# Function to customize PS1 when using powerline-shell.
# C.f. https://github.com/banga/powerline-shell for more info
# Since preferred font is "Cousine for Powerline" (not "Cousine" !), install it from https://github.com/powerline/fonts
# Just select it in iTerm, in the current / default profile
function _update_ps1() {
  PS1="$(~/powerline-shell.py $? 2> /dev/null)"
}

# Set my favorite PS1 prompt
function setupPrompts () {
  myOut "Setting up shell prompts..."

  # Use powerline-shell.py if it exists, otherwise default to a simpler one
  local POWERLINE_SHELL_PY="${HOME}/powerline-shell.py"
  if [[ -f "${POWERLINE_SHELL_PY}" ]]; then
    myOut "${INDENT} Powerline found, using it"
    if [ "$TERM" != "linux" ]; then
      PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
    fi
  else
    local POWERLINE_URL="https://github.com/banga/powerline-shell"
    myErr "${INDENT} Powerline not found. You may want to install it from ${POWERLINE_URL} (and also install "Cousine for Powerline" font from https://github.com/powerline/font)"
    myOut "${INDENT} Using my default PS1 and PS2 instead."
    # Inits desired codes
    local BOLD='\[\e[1m\]'
    local INVERSE='\[\e[7m\]'
    local RESET='\[\e[0m\]'
    local ITALIC='\[\e[6m\]'
    case $TERM in
      xterm*)
        local TITLE='\[\e]0;\W\a\]' # Put working dir in title & icon
        ;;
      *)
        local TITLE=''
        ;;
    esac

    # Customize!
    PS1="${TITLE}${RESET}${INVERSE}${BOLD}\u@\h${RESET}:${INVERSE}\w${RESET}> "
    PS2="${RESET}> ${ITALIC}"
  fi
}

# Set up bash-completion
function setupBashCompletion() {
  myOut "Setting up bash-completion..."

  # Loads bash-completion, if installed, otherwise suggests to install it
  local BASH_COMPL="$(brew --prefix)/etc/bash_completion"
  if [[ -f "${BASH_COMPL}" ]]
  then
    # Loads completion..
    source "${BASH_COMPL}"
  else
    local BASH_COMPL_CMD="brew install bash-completion"
    myErr "Not found: ${BASH_COMPL}"
    myOut "${INDENT} You may want to install bash-completion through Homebrew package manager with:"
    myOut "${INDENT} ${BASH_COMPL_CMD}"
  fi
}

# Set up git
function setupGit() {
  myOut "Setting up git..."

  if brew ls --versions git > /dev/null
  then
    # Git is already installed !
    myOut "${INDENT} Git has been already installed with Homebrew package manager."
  else
    # Git not installed with brew
    local GIT_CMD="brew install git"
    myErr "Git is not installed with brew, version is \"`git --version`\""
    myOut "${INDENT} You may want to install git through Homebrew package manager with (otherwise, completions will not work):"
    myOut "${INDENT} ${GIT_CMD}"
  fi
}

# Set up NodeJS
function setupNode() {
  myOut "Setting up Node env..."

  # Is NodeJS installed ?
  if [[ `getOS` == 'osx' ]]
  then
    export NVM_DIR="$HOME/.nvm"
    if [[ -d "${NVM_DIR}" ]]
    then
      local NVM_SH="$NVM_DIR/nvm.sh"
      local NVM_UPGRADE_CMD='( cd "$NVM_DIR"; git fetch origin; git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" origin`; ) && . "$NVM_DIR/nvm.sh"'
      local NVM_COMP="$NVM_DIR/bash_completion"
      myOut "${INDENT} Sourcing $NVM_SH..."
      . "${NVM_SH}"
      myOut "${INDENT} Reminder: to manually upgrade, run: ${NVM_UPGRADE_CMD}"
      myOut "${INDENT} Sourcing bash completions ${NVM_COMP}..."
      [[ -r ${NVM_COMP} ]] && . ${NVM_COMP} || myErr "Not found: ${NVM_COMP}"
    else
      local NVM_INSTALL_CMD='export NVM_DIR="$HOME/.nvm" && mkdir "$NVM_DIR" && ( git clone https://github.com/creationix/nvm.git "$NVM_DIR"; cd "$NVM_DIR"; git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" origin`;) && . "$NVM_DIR/nvm.sh"'
      myErr "Not found: ${NVM_DIR}"
      myOut "${INDENT} You may want to install nvm first, by running: "
      myOut "${INDENT} ${NVM_INSTALL_CMD}"
    fi
  else
    myErr "Not checking node env on non-macOS systems, sorry."
  fi
}

# Set up homeshick
function setupHomeshick () {
  myOut "Setting up homeshick..."

  # Some constants
  local HOMESHICK="${HOME}/.homesick/repos/homeshick/homeshick.sh"

  # Go through setup...
  if [[ -f "${HOMESHICK}" ]]; then
    # Loads homeshick and its completion, plus refresh repos...
    local HOMESHICK_COMPL="${HOME}/.homesick/repos/homeshick/completions/homeshick-completion.bash"
    source "${HOMESHICK}"
    source "${HOMESHICK_COMPL}"

    myOut "${INDENT} homeshick --quiet refresh..."
    homeshick --quiet refresh

    # Check whether we need to... "homeshick check"
    local file="$HOME/.lastHomeshickCheckTimestamp"

    # Already checked in last 24h or not?
    if [[ ! -f "$file" || $(find "$file" -mtime +1 -print) ]]; then
      myOut "${INDENT} homeshick check..."
      homeshick check
      [[ ! -f "$file" ]] && myOut "${INDENT} Future \"homeshich check\" will be done after 24h, using file's timestamp: \"$file\"..."
      touch "$file"
    else
      myOut "${INDENT} homeshick check already done in last 24h, no need to check again..."
    fi
  else
    myErr "${INDENT} Not found: ${HOMESHICK}"
    # Clone it now ?
    local GITCMD="git clone git://github.com/andsens/homeshick.git \"${HOME}/.homesick/repos/homeshick\""
    if [[ $(ask "${INDENT} Clone it from github now?" "[Y|n]" Y) == Y ]]; then
      eval "${GITCMD}" && echo && myOut "${INDENT} You will have to source \"${HOMESHICK}\" and \"${HOMESHICK_COMPL}\" manually" && echo
    else
      myOut "${INDENT} You may want to clone it later with following command:"
      myOut "${INDENT} ${GITCMD}"
    fi
  fi
}

# Set up aliases
function setupAliases () {
  myOut "Setting up aliases..."

  # Loading aliases if they exist...
  local ALIASES="${HOME}/.aliases"
  if [[ -f "${ALIASES}" ]]; then
    source "${ALIASES}"
  else
    myErr "Not found: ${ALIASES}. Consider restoring aliases."
  fi
}

# Set some env vars
function setupEnvVars () {
  myOut "Setting up environment variables..."

  # Set up the PAGER
  export PAGER=/usr/bin/less

  # Set up the PATHs
  [[ -d $HOME/.npm-packages/bin ]] && export PATH="$PATH:$HOME/.npm-packages/bin"
  [[ -d $HOME/bin ]] && export PATH="$HOME/bin:$PATH"

  # OSX specific env vars
  if [[ `getOS` == 'osx' ]]
  then
    # Emacs
    myOut "${INDENT} Choosing EDITOR to use..."

    # Do we have emacs in $HOME/bin ?
    local EMACS="$HOME/bin/emacs"
    if [[ ! -x "$EMACS" ]]
    then
      EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
    fi

    # Set EDITOR
    export EDITOR="$EMACS"

    # Now, emacsclient
    local EMACSCLIENT="$HOME/bin/ec"
    if [[ ! -x "$EMACSCLIENT" ]]
    then
      EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/Emacs"
    fi

    # if [[ -x "$EMACSCLIENT" ]]
    # then
    #   myOut "${INDENT} Starting emacs daemon..."
    #   "$EMACS" --daemon 2>&1 | while read line
    #   do
    #     myOut "\t${INDENT} emacs daemon says: ($line)"
    #   done
    #   if [[ "$?" -eq 0 ]]
    #   then
    #     export EDITOR="$EMACSCLIENT -c"
    #   else
    #     myErr "${INDENT} Cannot start emacs as a daemon"
    #     export EDITOR="$EMACS"
    #   fi
    #   myOut "${INDENT} Env var EDITOR is now \"$EDITOR\""
    #   alias emacs="$EDITOR"
    #   myOut "${INDENT} Alias  'emacs' is overriden to \"$EDITOR\" as well"
    # fi

    # I had to use Groovy sometimes...
    local GHOME="$(brew --prefix)/opt/groovy/libexec"
    [[ -d "$GHOME" ]] && export GROOVY_HOME="$GHOME"

    # This is to setup the RuvyGems env, for sudo-less cocoapods
    export GEM_HOME=$HOME/.gem
    export PATH=$PATH:$GEM_HOME/ruby/2.0.0/bin

    # I even tried Android Studio at some point :)
    local AHOME="$HOME/Library/Android/sdk"
    [[ -d "$AHOME" ]] && export ANDROID_HOME="$AHOME" && export PATH="$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools"
  fi

  # Loads the local custom add-on when it exists
  if [[ -f "$HOME/.profile.local" ]]
  then
    source "$HOME/.profile.local"
  fi
}

# Main entry point, because I like unique entry point
function main () {
  myOut "Starting bash setup..."

  # Cygwin has a particular setup...
  [[ "$(uname -s)" == "CYGWIN*" ]] && setupCygwin

  # Now set all up!
  local INDENT="  \033[2m==>"
  setupAliases
  setupEnvVars
  setupHomeshick
  setupNode
  setupGit
  setupBashCompletion
  setupPrompts

  # Umask: neither group nor others have any perms:
  umask 077
}

# Let's go
main
