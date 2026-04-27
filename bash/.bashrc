#
# Name:
#   ~/.bashrc
#
# Purpose:
#
#  Part of my dotfiles https://github.com/indiebrain/.files
#
#  Configure Interactive Shells. See the GNU Bash manuals for
#  information on startup files:
#  https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.htlm.

# Add locations to the command lookup PATH

## Add my scripts to the command lookup PATH
if [ -d "$HOME"/bin ]
then
    PATH=$PATH:"$HOME"/bin
fi

if [ -d "$HOME"/.local/bin ]
then
    PATH=$PATH:"$HOME"/.local/bin
fi

## Add usr binaries to the command lookup PATH
if [ -d /usr/local/bin ]
then
    PATH=$PATH:/usr/local/bin
fi

if [ -d /usr/local/sbin ]
then
    PATH=$PATH:/usr/local/sbin
fi

# Add locations the the cd command's lookup path. Usually this is just
# the current directory, but here I add places on the file system that
# I frequently access to save myself a few keystrokes. See
# https://www.gnu.org/software/bash/manual/bash.html#index-cd
export CDPATH=".:$HOME/Developer/"

# Command History

## Always append commands to the history
shopt -s histappend

## After command execution, immediately write and reload the history file into the shell session
export PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

## Prevent duplicate history entryies
export HISTCONTROL=ignoreboth:erasedups

## Configure the amount of history to keep
export HISTSIZE=100000
export HISTFILESIZE=10000000

## Never write commands that match these expressions to the history
export HISTIGNORE=bg*:cd*:clear*:exit*:fg*:ll*:ls*:pwd*:history*

## XDG Config
export XDG_CONFIG_HOME=${HOME}/.config

# Aliases
alias gs="git status"
alias gl='git log --graph --oneline --decorate --max-count 10'

# Default pager. The check for the terminal is useful for Emacs with
# M-x shell (which is how I usually interact with bash these days).
#
# The COLORTERM is documented in (info "(emacs) General Variables") -
# https://www.gnu.org/software/emacs/manual/html_node/emacs/General-Variables.html.
# I found the reference to `dumb-emacs-ansi' in (info "(emacs)
# Connection Variables").
if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ]
then
    export PAGER="cat"
    alias less="cat"
    export COLORTERM=1
else
    # Quit once you try to scroll past the end of the file.
    export PAGER="less --quit-at-eof"
fi

export MANPAGER="$PAGER"

# Setup the default editor
# Default editor.
export VISUAL="emacsclient-editor"
export EDITOR="emacsclient-editor"

# Setup the Primary Prompt String - $PS1. See the GNU Bash manual's
# "Controlling the Prompt" section:
# https://www.gnu.org/software/bash/manual/bash.html#Controlling-the-Prompt

## Setup color short-hand variables
if [ -n "$COLORTERM" ] || [[ "$CLICOLOR" -gt 0 ]]
then
    # When the terminal supports colors, setup shorthands for the ANSI
    # color codes.
    ESC="\033"
    RED="\[$ESC[01;31m\]"
    LIGHT_RED="\[$ESC[1;31m\]"
    GREEN="\[$ESC[01;32m\]"
    LIGHT_GREEN="\[$ESC[1;32m\]"
    YELLOW="\[$ESC[0;33m\]"
    LIGHT_YELLOW="\[$ESC[1;33m\]"
    BLUE="\[$ESC[0;34m\]"
    LIGHT_BLUE="\[$ESC[1;34m\]"
    MAGENTA="\[$ESC[0;35m\]"
    LIGHT_MAGENTA="\[$ESC[1;35m\]"
    CYAN="\[$ESC[0;36m\]"
    LIGHT_CYAN="\[$ESC[1;36m\]"
    WHITE="\[$ESC[0;37m\]"
    LIGHT_WHITE="\[$ESC[1;37m\]"
    GREY="\[$ESC[0;37m\]"

    RESET="\[$ESC[0m\]"
else
    # When the terminal DOES NOT supports colors, all colors are the empty string to prevent goofy escape sequences muddying up the output.
    ESC=""
    RED=""
    LIGHT_RED=""
    GREEN=""
    LIGHT_GREEN=""
    YELLOW=""
    LIGHT_YELLOW=""
    BLUE=""
    LIGHT_BLUE=""
    MAGENTA=""
    LIGHT_MAGENTA=""
    CYAN=""
    LIGHT_CYAN=""
    WHITE=""
    LIGHT_WHITE=""
    GREY=""
    RESET=""
fi

## Get a graphical representation of the clean/dirty state of a git repository

### Include git ps1 helpers

#### Arch Linux
[[ -f /usr/share/git/completion/git-prompt.sh ]] && source  /usr/share/git/completion/git-prompt.sh

### Configure git PS1
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWUPSTREAM="verbose"
export GIT_PS1_SHOWCONFLICTSTATE="yes"

__git_prompt() {

    local last_exit="$?" # keep here.. so we get the last command

    # setup PS1
    local timestamp="${BLUE}\t${RESET}"
    local working_dir="${CYAN}\W${RESET}"
    local git_info="$(__git_ps1 "(%s)")"
    PS1="[$timestamp] $working_dir $git_info"

    # setup marker that acts off of last exit code
    local marker_color
    if [ 0 -eq "$last_exit" ]; then
        marker_color="$GREEN"
    else
        marker_color="$RED"
    fi
    local marker="${marker_color}\$${RESET}"
    PS1="\n${PS1} →\n${marker} "
}
PROMPT_COMMAND=__git_prompt

# Spring Gem
DISABLE_SPRING=1

# Golang

## Store golang binaries outside of the standard HOME directory, and make sure they're on the command lookup PATH
export GOPATH=$HOME/Developer/go
export PATH=$PATH:$GOPATH/bin

# MacOS Homebrew "Package Manager"
[[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)
export HOMEBREW_NO_ANALYTICS=1
if type brew &>/dev/null; then
    HOMEBREW_PREFIX="$(brew --prefix)"
    if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
        source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
    else
        for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
            [[ -r "$COMPLETION" ]] && source "$COMPLETION"
        done
    fi
fi

# Ripgrep
[ -f $HOME/.ripgreprc ] && export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# asdf-vm tool manager
if type asdf &> /dev/null; then
    export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
    . <(asdf completion bash)
fi

# mise tool manager
if type mise &> /dev/null; then
    eval "$(mise activate bash)"
fi

# Stable K9S configuration directory across hosts
export K9S_CONFIG_DIR=~/.config/k9s

# Ollama config

## flash Attention is a feature of most modern models that can
## significantly reduce memory usage as the context size grows.
export OLLAMA_FLASH_ATTENTION=1

## Specifies the quantization type for the K/V (Key/Value) cache. This
## setting is crucial for optimizing memory usage when running large
## language models (LLMs). Supported Quantization Types
##
## The currently available K/V cache quantization types are:
##
##     - f16 - high precision and memory usage (default).
##
##     - q8_0 - 8-bit quantization, uses approximately 1/2 the memory
##       of f16 with a very small loss in precision, this usually has
##       no noticeable impact on the model’s quality (recommended if
##       not using f16).
##
##     - q4_0 - 4-bit quantization, uses approximately 1/4 the memory
##       of f16 with a small-medium loss in precision that may be more
##       noticeable at higher context sizes.
##
export OLLAMA_KV_CACHE_TYPE=q8_0

## The maximum time allowed for a request to complete in the Ollama
## API: default 30s
export OLLAMA_REQUEST_TIMEOUT=300s

## How long models remain loaded in memory when idle: default: 5
## minutes.
export OLLAMA_KEEP_ALIVE=15m

## Controls the maximum number of models that can be loaded into
## memory at the same time when using the Ollama service
export OLLAMA_MAX_LOADED_MODELS=2

# Per-host shell configuration overrides
[ -f $HOME/.bashrc.local ] && source $HOME/.bashrc.local
