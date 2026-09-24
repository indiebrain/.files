# Your ~/.bashrc (from the dotfiles) replaces Omarchy's, and it already sources
# ~/.bashrc.local. Keep a managed block there that plugs Omarchy back in, so the
# dotfiles stay portable to macOS/Debian.
# shellcheck shell=bash

{
  cat <<'BLOCK'
# Omarchy environment: OMARCHY_PATH, omarchy-* on PATH, mise shims.
[[ -r /usr/share/omarchy/default/bash/env-bootstrap ]] && source /usr/share/omarchy/default/bash/env-bootstrap
BLOCK
  if [[ ${OMARCHY_BASH:-env} == "full" ]]; then
    cat <<'BLOCK'
# Omarchy aliases, functions, completions and prompt tools.
[[ $- == *i* && -r $OMARCHY_PATH/default/bash/rc ]] && source "$OMARCHY_PATH/default/bash/rc"
BLOCK
  else
    cat <<'BLOCK'
# Bash completion for the tools Omarchy installs (prompt and aliases stay yours).
[[ $- == *i* && -r /usr/share/bash-completion/bash_completion ]] && source /usr/share/bash-completion/bash_completion
BLOCK
  fi
} | ensure_block "$HOME/.bashrc.local" "omarchy"
