# Install mise-backed CLI wrappers listed in packages/mise.tools, via Omarchy's
# own `omarchy-mise-install`. Runs after the remove module, which deletes
# Omarchy's preinstalled wrappers, so the ones you use come straight back.
# shellcheck shell=bash

while read -r -a tool; do
  (( ${#tool[@]} > 0 )) || continue
  command_name="${tool[1]:-${tool[0]}}"
  if [[ -x $HOME/.local/bin/$command_name ]]; then
    continue
  fi
  log "Installing $command_name (mise wrapper)"
  run omarchy-mise-install "${tool[@]}"
done < <(read_manifest "$OVERLAY_DIR/packages/mise.tools")
