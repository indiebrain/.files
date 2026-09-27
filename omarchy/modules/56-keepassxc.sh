# Why a hand-written manifest and what "never ask" costs: docs/cir/CIR-004-keepassxc-browser-integration.md
# shellcheck shell=bash

if [[ ${KEEPASSXC_BROWSER_INTEGRATION:-false} == "true" ]]; then
  keepassxc_config="${KEEPASSXC_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/keepassxc/keepassxc.ini}"
  keepassxc_manifest="${KEEPASSXC_MANIFEST:-$HOME/.mozilla/native-messaging-hosts/org.keepassxc.keepassxc_browser.json}"

  # Set key=value under [section], leaving every other line of the file alone.
  ini_set() {
    local file="$1" section="$2" key="$3" value="$4" tmp
    if [[ $(ini_get "$file" "$section" "$key") == "$value" ]]; then
      return 0
    fi
    tmp="$(mktemp)"
    # Blank lines are held back so a new key joins the end of its section's
    # keys rather than the gap before the next section.
    awk -v section="[$section]" -v key="$key" -v value="$value" '
      function flush(   i) { for (i = 0; i < blanks; i++) print ""; blanks = 0 }
      /^[ \t]*$/ { blanks++; next }
      /^\[/ {
        if (in_section && !written) { print key "=" value; written = 1 }
        flush()
        in_section = ($0 == section)
        print; lines++
        next
      }
      in_section && $0 ~ "^" key "[ ]*=" { flush(); print key "=" value; written = 1; lines++; next }
      { flush(); print; lines++ }
      END {
        if (written) { flush() }
        else if (in_section) { print key "=" value; flush() }
        else { flush(); if (lines) print ""; print section; print key "=" value }
      }
    ' <(cat "$file" 2>/dev/null) >"$tmp"
    log "KeePassXC: $section/$key=$value"
    run mkdir -p "$(dirname "$file")"
    run install -m 600 "$tmp" "$file"
    rm -f "$tmp"
  }

  ini_get() {
    local file="$1" section="$2" key="$3"
    awk -v section="[$section]" -v key="$key" '
      /^\[/ { in_section = ($0 == section); next }
      in_section && $0 ~ "^" key "[ ]*=" { sub("^" key "[ ]*=[ ]*", ""); print; exit }
    ' <(cat "$file" 2>/dev/null)
  }

  if pgrep -x keepassxc >/dev/null 2>&1; then
    warn "KeePassXC is running and rewrites its config on exit; quit it before trusting these settings"
  fi

  ini_set "$keepassxc_config" Browser Enabled true
  if [[ ${KEEPASSXC_ALWAYS_ALLOW_ACCESS:-false} == "true" ]]; then
    ini_set "$keepassxc_config" Browser AlwaysAllowAccess true
  fi

  # Firefox decides a native messaging host exists by finding this file, and
  # KeePassXC decides the browser is enabled the same way.
  proxy="$(command -v keepassxc-proxy 2>/dev/null || echo /usr/bin/keepassxc-proxy)"
  manifest_tmp="$(mktemp)"
  cat >"$manifest_tmp" <<MANIFEST
{
    "allowed_extensions": [
        "keepassxc-browser@keepassxc.org"
    ],
    "description": "KeePassXC integration with native messaging support",
    "name": "org.keepassxc.keepassxc_browser",
    "path": "$proxy",
    "type": "stdio"
}
MANIFEST
  if ! cmp -s "$manifest_tmp" "$keepassxc_manifest"; then
    log "KeePassXC: registering the native messaging host for Firefox"
    run mkdir -p "$(dirname "$keepassxc_manifest")"
    run install -m 644 "$manifest_tmp" "$keepassxc_manifest"
  fi
  rm -f "$manifest_tmp"
fi
