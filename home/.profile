# My bash profile, normally hosted at https://github.com/maanuair/dotfiles

# Some log helpers
function myInfo () {
    printf "%s [DOT-INFO] ${@}\n" `date "+%H:%M:%S"`
}
function myError () {
    printf "\r\033[2K`date "+%H:%M:%S"` [\033[0;31mDOT-ERROR\033[0m] ${@}\n" 1>&2
}
function myDebug () {
    printf "\r\033[2K`date "+%H:%M:%S"` [\033[0;33mDOT-DEBUG\033[0m] ${@}\n"
}

# Loads aliases
source "$HOME/.bash_aliases"

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
	myError "Function $FUNCNAME(): bad arguments count, ignored."
	myError "Synopsis: $FUNCNAME \"So we quit?\" \"[y|yes|Y|n|no|N]\" [\"N\"]"
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
    # We want to use Windows native symbolink links on Cygwin!
    export CYGWIN="winsymlinks:nativestrict ${CYGWIN}"
    
    # But we must be Windows Administrator to make them work...
    id -G | grep -qE '\<(544|0)\>'; # Group 544 or 0 does mean Windows Administrator
    if [[ $? != 0 ]]; then
	echo "You must run Cygwin as Administrator for native symbolic links available."
	if [[ $(ask "Continue anyway?" "[y|N]" N) == N ]]; then
	    exit 1;
	fi;
    fi
}

# Set up homeshick
function setupHomeshick () {
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
	echo "Not found: homeshick.sh"
	# Clone it now ?
	if [[ $(ask "Clone it from github now?" "[Y|n]" Y) == Y ]]; then
	    eval "${GITCMD}" && echo && echo "You will have to source \"${HOMESHICK}\" and \"${HOMESHICK_COMPL}\" manually" && echo
	else
	    echo "You may want to clone it later with following command:"
	    echo "  ${GITCMD}"
	fi
    fi
}

# Set up aliases
function setupAliases () {
    local ALIASES="${HOME}/.bash_aliases"
    if [[ -f "${ALIASES}" ]]; then
	source "${ALIASES}"
    else
	echo "Not found: ${ALIASES}"
    fi
}

# Function to customize PS1 when using powerline-shell.
# C.f. https://github.com/banga/powerline-shell for more info, or c.f  https://github.com/powerline/fonts for compatible fonts
# NB: Preferred font is Cousine
function _update_ps1() {
    PS1="$(~/powerline-shell.py $? 2> /dev/null)"
}

# Set my favorite PS1 prompt
function setPrompts () {
    # Use powerline-shell.py if it exists, otherwise default to a simpler one
    if [[ -f "${HOME}/powerline-shell.py" ]]; then
	if [ "$TERM" != "linux" ]; then
	    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
	fi
    else
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

# Set some env vars
function setEnvVars () {
    # Common env vars
    export PATH="$PATH:$HOME/.npm-packages/bin"
    export PAGER=/usr/bin/less
    
    # OSX specific env vars
    if [[ "$(uname -s)" == "Darwin" ]]
    then
	export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs"
    fi
}
    
# Main entry point, because I like unique entry point
function main () {
    myInfo "Running bash setup..."

    # Cygwin has a particular setup
    [[ "$(uname -s)" == "CYGWIN*" ]] && setupCygwin

    # Set up some env vars
    setEnvVars
    
    # Source aliases
    setupAliases
	
    # Set up homeshick
    setupHomeshick
        
    # Customize my prompts
    setPrompts

    # Umask: neither group nor others have any perms:
    umask 077
}

# Let's go
main
