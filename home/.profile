# My bash profile, normally hosted at https://github.com/maanuair/dotfiles

# Some log helpers
function myOut () {
    printf "%s [\033[34mDOT-INFOR\033[0m] ${@}\033[0m\n" `date "+%H:%M:%S"`
}

function myErr () {
    printf "%s [\033[31mDOT-ERROR\033[0m] ${@}\033[0m\n" `date "+%H:%M:%S"` 1>&2
}

function myDebug () {
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
    
    local POWERLINE_URL="https://github.com/banga/powerline-shell"
    local POWERLINE_SHELL_PY="${HOME}/powerline-shell.py"
    
    # Use powerline-shell.py if it exists, otherwise default to a simpler one
    if [[ -f "${POWERLINE_SHELL_PY}" ]]; then
	myOut "${INDENT} Powerline found, using it"
	if [ "$TERM" != "linux" ]; then
	    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
	fi
    else
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
    
    local BASH_COMPL="$(brew --prefix)/etc/bash_completion"
    local BASH_COMPL_CMD="brew install bash-completion"

    # Loads bash-completion, if installed, otherwise suggests to install it
    if [[ -f "${BASH_COMPL}" ]]
    then
	# Loads completion..
	source "${BASH_COMPL}"
    else
	myErr "Not found: ${BASH_COMPL}"
	myOut "${INDENT} You may want to install bash-completion through Homebrew package manager with:"
	myOut "${INDENT} ${BASH_COMPL_CMD}"
    fi
}

# Set up git
function setupGit() {
    myOut "Setting up git..."

    local GIT_CMD="brew install git"

    if brew ls --versions git > /dev/null
    then
	# Git is already installed !
	myOut "${INDENT} Git has been already installed with Homebrew package manager."
    else
	# Git not installed with brew
	myErr "Git is not installed with brew, version is \"`git --version`\""
	myOut "${INDENT} You may want to install git through Homebrew package manager with (otherwise, completions will not work):"
	myOut "${INDENT} ${GIT_CMD}"
    fi
}

# Set up password store, c.f. https://www.passwordstore.org
function setupPass() {
    local PASSWORDSTORE_COMPL="/usr/local/etc/bash_completion.d/pass"
    local PASSWORDSTORE_CMD="brew install pass"
    
    # Loads password store completion, if installed, otherwise suggests install it
    if [[ -f "${PASSWORDSTORE_COMPL}" ]]
    then
	# Loads completion..
	source "${PASSWORDSTORE_COMPL}"
    else
	myErr "Not found: ${PASSWORDSTORE_COMPL}"
	myOut "${INDENT} You may want to install password store through Homebrew package Manager with:"
	myOut "${INDENT} ${PASSWORDSTORE_CMD}"
	myOut "${INDENT} echo \"source /usr/local/etc/bash_completion.d/pass\" >> ~/.bashrc"
    fi
}

# Set up homeshick
function setupHomeshick () {
    myOut "Setting up homeshick..."
    
    # Some constants
    local HOMESHICK="${HOME}/.homesick/repos/homeshick/homeshick.sh"
    local HOMESHICK_COMPL="${HOME}/.homesick/repos/homeshick/completions/homeshick-completion.bash"
    local GITCMD="git clone git://github.com/andsens/homeshick.git \"${HOME}/.homesick/repos/homeshick\""

    # Go through setup...
    if [[ -f "${HOMESHICK}" ]]; then
	# Loads homeshick and its completion, plus refresh repos...
	source "${HOMESHICK}"
	source "${HOMESHICK_COMPL}"
	homeshick --quiet refresh
    else
	myErr "${INDENT} Not found: ${HOMESHICK}"
	# Clone it now ?
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

    local ALIASES="${HOME}/.bash_aliases"

    if [[ -f "${ALIASES}" ]]; then
	source "${ALIASES}"
    else
	myErr "Not found: ${ALIASES}. Consider restoring aliases."
    fi
}

# Set some env vars
function setupEnvVars () {
    myOut "Setting up environment variables..."

    # Common env vars
    export PATH="$PATH:$HOME/.npm-packages/bin"
    export PAGER=/usr/bin/less
    
    # OSX specific env vars
    if [[ "$(uname -s)" == "Darwin" ]]
    then
	# Emacs
	local EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
	[ -x "$EMACS" ] && export EDITOR="$EMACS"

	# I had to use Groovy sometimes...
	local GHOME="$(brew --prefix)/opt/groovy/libexec"
	[[ -d "$GHOME" ]] && export GROOVY_HOME="$GHOME" 
    fi
}
    
# Main entry point, because I like unique entry point
function main () {
    myOut "Starting bash setup..."

    # Cygwin has a particular setup...
    [[ "$(uname -s)" == "CYGWIN*" ]] && setupCygwin

    # Now set all up!
    local INDENT="  \033[2m==>"
    setupEnvVars
    setupAliases
    setupHomeshick
    setupPass
    setupBashCompletion
    setupPrompts

    # Umask: neither group nor others have any perms:
    umask 077
}

# Let's go
main
