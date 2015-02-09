# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Function to source the list of files given in parameters
# Does not source wwen not existing in home dir
function sourceHomeFiles () {
    for file in $@; do
	local f="${HOME}/${file}"
	[[   -f "${f}" ]] && echo -ne "Sourcing ${f}... " && source "${f}" && [[ $? -eq 0 ]] && echo "OK"
	[[ ! -f "${f}" ]] && echo     "NOT sourcing ${f}: nonexistent"
    done
}

# Set my favorite PS1 prompt
function setPrompts () {
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
    PS1="${TITLE}${RESET}${BOLD}\u@\h${RESET}:${INVERSE}\w${RESET}\$ "
    PS2="${RESET}> ${ITALIC}"
}


# Source my files!
sourceHomeFiles ".myAliases"

# Customize my prompts
setPrompts

# Umask: neither group nor others have any perms:
umask 077

