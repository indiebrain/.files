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
if pgrep -x emacs > /dev/null
then
    export VISUAL="emacsclient -c"
    export EDITOR="emacsclient -t"
else
    export VISUAL=vim
    export EDITOR=$VISUAL
fi

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

### prints path to git directory
__git_dirname() {
    local dirname
    if [ -d .git ]; then
        dirname=".git"
    else
        dirname="$(git rev-parse --git-dir 2>/dev/null)"
    fi
    echo "$dirname"
}

### gets the branching state of the repository
### optional arg: path to git directory
__git_branching_state() {
    local gitdir="$1" || "$(__git_dirname)"
    local state

    if [ -f "$gitdir/rebase-merge/interactive" ]; then
        state="rebase-i"
    elif [ -d "$gitdir/rebase-merge" ]; then
        state="rebase-m"
    else
        if [ -d "$gitdir/rebase-apply" ]; then
            if [ -f "$gitdir/rebase-apply/rebasing" ]; then
                state="rebase"
            elif [ -f "$gitdir/rebase-apply/applying" ]; then
                state="am"
            else
                state="am/r"
            fi
        elif [ -f "$gitdir/MERGE_HEAD" ]; then
            state="merge" # merging
        elif [ -f "$gitdir/BISECT_LOG" ]; then
            state="bisect" # bisecting
        fi
    fi
    echo "$state"
}

### prints the working directory state of the repository using symbols
### these could be expensive.. would make sense to have configs to turn off
### * - modified / + - staged / ^ - stashed / % - untracked
__git_working_dir_symbols() {
    local symbols

    # in working dir
    if [ true = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]; then
        git diff --no-ext-diff --quiet --exit-code || symbols="*"
        if git rev-parse --quiet --verify HEAD >/dev/null; then
            git diff-index --cached --quiet HEAD -- || symbols="${symbols}+"
        fi
    fi

    # stashed
    git rev-parse --verify refs/stash >/dev/null 2>&1 && symbols="${symbols}^"

    # untracked files
    if [ -n "$(git ls-files --others --exclude-standard)" ]; then
        symbols="${symbols}%"
    fi
    echo "$symbols"
}

### prints current / parent branch name
### optional arg: 'parent' -- returns a limited guess of the parent
### optional arg: path to git directory
__git_branch_name() {
    # current branch name
    local branch
    local gitdir="$2" || "$(__git_dirname)"
    branch="$(git symbolic-ref HEAD 2>/dev/null)" || {
        branch="$(git describe --contains --all HEAD 2>/dev/null)" ||
            branch="$(cut -c1-7 "$gitdir/HEAD" 2>/dev/null)..." ||
            branch="unknown"

        branch="${branch##remotes/}"
        branch="($branch)"
    }
    branch="${branch##refs/heads/}"

    # parent branch name
    if [ parent = "$1" ]; then
        if [ master = "$branch" ]; then
            local refs="$(git for-each-ref --format="%(refname:short)")"
            case "$refs" in
                *git-svn*) # git-svn repo
                    branch='git-svn' ;;
                *origin*) # remote clone
                    branch='origin' ;;
                *)
                    branch='HEAD' ;; # same repo
            esac
        else
            # TODO.. would be nice to improve this to determine the actual
            # merge base (git merge-base) and compare against that instead of
            # always assuming master. In this way a 'topic/feature' branch
            # would show the diff counts for its parent 'next/develop' branch
            # rather than those plus those on the 'next/develop' branch.
            # I don't think we want to loop over the refs comparing ... that's
            # fuzzy.
            branch='master' # on a branch
        fi
    fi
    echo "$branch"
}

### prints if inside git directory or bare git repository
__git_in_gitdir() {
    if [ true = "$(git rev-parse --is-inside-git-dir 2>/dev/null)" ]; then
        if [ true = "$(git rev-parse --is-bare-repository 2>/dev/null)" ]; then
            echo 'bare'
        else
            echo 'gitdir'
        fi
    fi
}

### prints number of commits that are available on ref B but not ref A
### arg1: reference A
### arg2: reference B
__git_commit_diff_count() {
    echo "$(git rev-list $1..$2 2>/dev/null | awk 'END {print NR}')"
}

### build combined (+/-) counts for related commits
__git_count_str() {
    local str
    local parent="$(__git_branch_name parent)"
    local ahead_count="$(__git_commit_diff_count $parent HEAD)"
    local behind_count="$(__git_commit_diff_count HEAD $parent)"

    if [ 0 -lt "$ahead_count" ]; then
        str="${GREEN}+${ahead_count}${RESET}"
    fi

    if [ 0 -lt "$behind_count" ]; then
        [ -n "$str" ] && str="$str/"
        str="${str}${RED}-${behind_count}${RESET}"
    fi

    [ -n "$str" ] && str="($str)"
    echo "$str"
}

### install git integration into PS1
__git_prompt() {
    local last_exit="$?" # keep here.. so we get the last command

    # setup PS1
    local host="${MAGENTA}\h:${RESET}"
    local dir="${CYAN}\W${RESET}"
    PS1="[$host $dir]"

    # when in git repository
    local gitdir="$(__git_dirname)"
    if [ -n "$gitdir" ]; then
        local branch
        local extras

        local in_gitdir="$(__git_in_gitdir)"
        case "$in_gitdir" in
            gitdir|bare)
                branch="~$(echo $in_gitdir | tr "[:lower:]" "[:upper:]")~"
                extras=""
                ;;
            *)
                local branch="$(__git_branch_name current ${gitdir})"
                local br_state="$(__git_branching_state $gitdir)"

                # rebasing..use merge head for branch name
                case "$br_state" in
                    rebase-*)
                        # get the ref head during rebase
                        branch="$(cat "$gitdir/rebase-merge/head-name")"
                        branch="${branch##refs/heads/}"
                        branch="${branch##remotes/}"
                        ;;
                esac

                # extras (count strings, working dir symbols)
                local countstr="$(__git_count_str)"
                local wd_syms="${LIGHT_MAGENTA}$(__git_working_dir_symbols)${RESET}"
                extras="${countstr} ${wd_syms}"
                ;;
        esac
        branch="${BLUE}${branch}${RESET}"

        # update PS1
        PS1="${PS1} ${branch}${extras}"
    fi

    # setup marker that acts off of last exit code
    local marker
    if [ 0 -eq "$last_exit" ]; then
        marker="$GREEN"
    else
        marker="$RED"
    fi
    marker="${marker}\$${RESET}"
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

# Podman

## Hijack the DOCKER_HOST env var so `docker *' commands are forwarded to podman
export DOCKER_HOST='unix://$HOME/.local/share/containers/podman/machine/qemu/podman.sock'

# Ripgrep
[ -f $HOME/.ripgreprc ] && export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# asdf-vm tool manager
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
. <(asdf completion bash)
