# Link the dotfiles into $HOME with stow. Files Omarchy seeded where a dotfile
# wants to go are backed up to ~/.local/state/dotfiles-omarchy/backups first.
#
# No git pull here: this runs from inside the repo, so updating is left to you.
# shellcheck shell=bash

stow_packages "$DOTFILES_DIR" "${DOTFILES_STOW[@]}"
[[ -d $DOTFILES_DIR/ssh/.ssh ]] && run chmod 700 "$DOTFILES_DIR/ssh/.ssh"
