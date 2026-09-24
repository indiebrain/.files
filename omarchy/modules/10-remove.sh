# Trim Omarchy defaults you don't use.
#
# 1. Preinstalls: delegate to Omarchy's own `omarchy-remove-preinstalls` so the
#    definition of "preinstalls" (web apps, TUI wrappers, mise stubs, apps)
#    follows upstream. It asks via `gum confirm`, so a shim answers yes.
# 2. Extras: anything listed in packages/remove.packages.
# shellcheck shell=bash

if [[ ${REMOVE_PREINSTALLS:-true} == "true" ]]; then
  log "Removing Omarchy preinstalls"
  if (( DRY_RUN )); then
    run omarchy-remove-preinstalls
  else
    shim_dir="$(mktemp -d)"
    # Answer "yes" to `gum confirm`; pass every other gum call through.
    real_gum="$(command -v gum || true)"
    cat >"$shim_dir/gum" <<SHIM
#!/bin/bash
[[ \${1:-} == confirm ]] && exit 0
exec "$real_gum" "\$@"
SHIM
    chmod +x "$shim_dir/gum"
    PATH="$shim_dir:$PATH" omarchy-remove-preinstalls || warn "omarchy-remove-preinstalls reported an error"
    rm -rf "$shim_dir"
  fi
fi

mapfile -t drop < <(read_manifest "$OVERLAY_DIR/packages/remove.packages")
if (( ${#drop[@]} > 0 )); then
  log "Removing packages: ${drop[*]}"
  run omarchy-pkg-drop "${drop[@]}"
fi
