# Point Omarchy's launchers (Super+Return, Super+Shift+Return, Super+Shift+N)
# at your apps, via Omarchy's own `omarchy-default-*` commands. Only calls them
# when the current value differs, so re-runs stay quiet.
# shellcheck shell=bash

set_default() {
  local kind="$1" want="$2" cmd="omarchy-default-$1" have
  [[ -n $want ]] || return 0
  if ! command -v "$cmd" >/dev/null 2>&1; then
    warn "$cmd not found; skipping default $kind"
    return 0
  fi
  have="$("$cmd" 2>/dev/null || true)"
  if [[ $have == "$want" ]]; then
    return 0
  fi
  log "Default $kind: ${have:-unset} -> $want"
  run "$cmd" "$want"
}

set_default terminal "${DEFAULT_TERMINAL:-}"
set_default editor "${DEFAULT_EDITOR:-}"

# Browsers Omarchy knows go through omarchy-default-browser; others (e.g.
# Firefox Developer Edition) are set with xdg-settings, which is exactly what
# omarchy-default-browser and omarchy-launch-browser use under the hood.
case "${DEFAULT_BROWSER:-}" in
  "") ;;
  chromium | chrome | brave | brave-origin | edge | firefox | zen)
    set_default browser "$DEFAULT_BROWSER"
    ;;
  *)
    browser_desktop="${DEFAULT_BROWSER_DESKTOP:-$DEFAULT_BROWSER.desktop}"
    current_browser="$(env -u BROWSER xdg-settings get default-web-browser 2>/dev/null || true)"
    if [[ $current_browser != "$browser_desktop" ]]; then
      log "Default browser: ${current_browser:-unset} -> $browser_desktop"
      run env -u BROWSER xdg-settings set default-web-browser "$browser_desktop"
    fi
    ;;
esac
