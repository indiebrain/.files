# Re-apply this overlay after every `omarchy update`, so upstream updates can't
# quietly bring back removed apps or clobber links.
# shellcheck shell=bash

hook_tmp="$(mktemp -d)"
hook_src="$hook_tmp/50-dotfiles-omarchy"
cat >"$hook_src" <<HOOK
#!/bin/bash
# Installed by dotfiles-omarchy. Re-asserts removals, packages and dotfiles.
"$OVERLAY_DIR/install" --hook
HOOK

hook_dest="$HOME/.config/omarchy/hooks/post-update.d/50-dotfiles-omarchy"
if ! { [[ -f $hook_dest ]] && cmp -s "$hook_src" "$hook_dest"; }; then
  log "Installing post-update hook"
  run omarchy-hook-install post-update "$hook_src"
fi
rm -rf "$hook_tmp"
