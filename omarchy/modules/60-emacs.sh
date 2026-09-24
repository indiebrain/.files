# Keep an Emacs daemon running (Arch's emacs ships emacs.service) so
# emacsclient-editor from the dotfiles opens frames instantly.
# shellcheck shell=bash

if [[ ${EMACS_DAEMON:-false} == "true" ]]; then
  if systemctl --user is-enabled --quiet emacs.service 2>/dev/null; then
    :
  elif systemctl --user cat emacs.service >/dev/null 2>&1; then
    log "Enabling emacs daemon (systemd user service)"
    run systemctl --user enable --now emacs.service
  else
    warn "emacs.service not found; is emacs installed?"
  fi
fi
