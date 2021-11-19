# Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Emmanuel Roubion
#
# Author: Emmanuel Roubion
# URL: https://github.com/maanuair/dotfiles

# This file is part of Emmanuel's Roubion dot files, released under
# the MIT License as published by the Massachusetts Institute of Technology
#
# These dotfiles are distributed in the hope they wil lbe useful, but
# without any warranty. See the MIT License for more details
#
# You should have received a copy of the MIT License along with this file.
# If not, see https://opensource.org/licenses/mit-license.php

# My bash profile

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
    myOutTab "Powerline found, using it"
    if [ "$TERM" != "linux" ]; then
      PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
    fi
  else
    local POWERLINE_URL="https://github.com/banga/powerline-shell"
    myErrTab "Powerline not found. You may want to install it from ${POWERLINE_URL} (and also install \"Cousine for Powerline\" font from https://github.com/powerline/font)"
    myOutTab "Using my default PS1 and PS2 instead."
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
  # local BASH_COMPL="$(brew --prefix)/etc/bash_completion"
  local BASH_COMPL="$(brew --prefix)/etc/profile.d/bash_completion.sh"
  if [[ -f "${BASH_COMPL}" ]]; then
      # Loads completion..
      source "${BASH_COMPL}"
  else
      local BASH_COMPL_CMD="brew install bash-completion"
      myErr "Not found: ${BASH_COMPL}"
      myOutTab "You may want to install bash-completion through Homebrew package manager with:"
      myOutTab "${BASH_COMPL_CMD}"
  fi
}

# Main entry point, because I like unique entry point
function main () {
  # Do we display our script output / debug info or not?
  myOutEnabled=false
  myDebugEnable=false

  # Load some custom shell functions
  source ~/.shell_functions
  myOut "Starting bash setup..."

  # Cygwin has a particular setup...
  [[ "$(uname -s)" == "CYGWIN*" ]] && setupCygwin

  # Now set all up!
  setupAliases
  setupEnvVars
  setupHomeshick
  # setupNode
  setupGit
  setupBashCompletion
  setupPrompts

  # Umask: neither group nor others have any perms:
  umask 077

  # Loads the local custom script when it exists
  sourceOrWarn "${HOME}/.shell_local_script"

  # iTerm2 integration
  sourceOrWarn "${HOME}/.iterm2_shell_integration.bash"
}

# Let's go
main
