# Install the software you use. Repo packages go through `omarchy-pkg-add`
# (pacman, skips what's already there); AUR packages through
# `omarchy-pkg-aur-add` (yay).
#
# One bad name makes pacman reject a whole batch, so on failure each package is
# retried alone and the ones that still fail are reported instead of aborting.
# shellcheck shell=bash

install_batch() {
  local installer="$1" label="$2"
  shift 2
  (( $# > 0 )) || return 0
  log "Installing $# $label packages"
  (( DRY_RUN )) && { run "$installer" "$@"; return 0; }
  "$installer" "$@" && return 0

  warn "batch install failed; retrying $label packages one at a time"
  local pkg failed=()
  for pkg in "$@"; do
    "$installer" "$pkg" >/dev/null 2>&1 || failed+=("$pkg")
  done
  (( ${#failed[@]} == 0 )) || warn "could not install: ${failed[*]} (check the names in packages/)"
}

mapfile -t pkgs < <(read_manifest "$OVERLAY_DIR/packages/install.packages")
install_batch omarchy-pkg-add repo "${pkgs[@]}"

mapfile -t aur < <(read_manifest "$OVERLAY_DIR/packages/aur.packages")
install_batch omarchy-pkg-aur-add AUR "${aur[@]}"
