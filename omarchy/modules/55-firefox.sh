# Why this destination and not /etc/firefox/policies: docs/cir/CIR-003-firefox-policy-configuration.md
# shellcheck shell=bash

if [[ ${FIREFOX_POLICIES:-false} == "true" ]]; then
  policies_src="$OVERLAY_DIR/firefox/policies.json"
  policies_dest="${FIREFOX_POLICIES_FILE:-/usr/lib/firefox-developer-edition/distribution/policies.json}"

  if ! python3 -c 'import json,sys; json.load(open(sys.argv[1]))' "$policies_src" 2>/dev/null; then
    die "$policies_src is not valid JSON; Firefox would ignore every policy in it"
  fi

  if ! cmp -s "$policies_src" "$policies_dest"; then
    log "Applying Firefox policies"
    run sudo install -D -m 644 "$policies_src" "$policies_dest"
    warn "Firefox applies policies at startup; restart it to pick these up"
  fi
fi
