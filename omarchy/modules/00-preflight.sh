# Make sure we're on Omarchy as a normal user and the tools the other modules
# lean on (git, stow) exist.
# shellcheck shell=bash

(( EUID != 0 || ${OVERLAY_ALLOW_ROOT:-0} )) || die "run as your normal user, not root (sudo is used where needed)"
require_cmd omarchy-pkg-add
require_cmd omarchy-pkg-drop

missing=()
for cmd in git stow; do
  command -v "$cmd" >/dev/null 2>&1 || missing+=("$cmd")
done
if (( ${#missing[@]} > 0 )); then
  log "Installing bootstrap tools: ${missing[*]}"
  run omarchy-pkg-add "${missing[@]}"
fi
