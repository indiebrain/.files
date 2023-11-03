[ -r ~/.profile ] && . ~/.profile || true              # If a Bourne shell configuration exists, load it
if [ -n "$PS1" ]                                         # Is this REALLY an interactive shell?
then
    [ -r ~/.bashrc ] && . ~/.bashrc || true            # tty/prompt/function setup for interactive shells
    [ -r ~/.bash_login ] && . ~/.bash_login || true    # any at-login tasks for login shell only
fi

export CLICOLOR=1

alias gs="git status"
alias gl='git log --graph --oneline --decorate --max-count 10'

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

export EDITOR="emacs -nw"

shopt -s histappend
export PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=100000
export HISTFILESIZE=10000000
export HISTIGNORE=bg*:cd*:clear*:exit*:fg*:ll*:ls*:pwd*:history*
