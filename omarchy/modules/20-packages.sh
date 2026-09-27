# Install the software you use. Repo packages go through `omarchy-pkg-add`
# (pacman, skips what's already there); AUR packages through
# `omarchy-pkg-aur-add` (yay).
# shellcheck shell=bash

mapfile -t pkgs < <(read_manifest "$OVERLAY_DIR/packages/install.packages")
pkg_batch omarchy-pkg-add "Installing repo packages" "${pkgs[@]}"

mapfile -t aur < <(read_manifest "$OVERLAY_DIR/packages/aur.packages")
pkg_batch omarchy-pkg-aur-add "Installing AUR packages" "${aur[@]}"
