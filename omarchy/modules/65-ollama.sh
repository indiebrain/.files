# Install Ollama with the right GPU build, apply the server settings from
# omarchy/ollama/ollama.service.conf, and run it as a system service.
#
# Build choice matches Omarchy's own menu: CUDA when nvidia-smi exists, ROCm
# when rocminfo exists, CPU otherwise. Pin one with OLLAMA_PACKAGE.
# shellcheck shell=bash

if [[ ${OLLAMA:-false} == "true" ]]; then
  ollama_pkg="${OLLAMA_PACKAGE:-}"
  if [[ -z $ollama_pkg ]]; then
    if command -v nvidia-smi >/dev/null 2>&1; then
      ollama_pkg="ollama-cuda"
    elif command -v rocminfo >/dev/null 2>&1; then
      ollama_pkg="ollama-rocm"
    else
      ollama_pkg="ollama"
    fi
  fi

  if ! command -v ollama >/dev/null 2>&1; then
    log "Installing $ollama_pkg"
    run omarchy-pkg-add "$ollama_pkg"
  fi

  dropin_src="$OVERLAY_DIR/ollama/ollama.service.conf"
  dropin_dest="${OLLAMA_DROPIN:-/etc/systemd/system/ollama.service.d/10-dotfiles.conf}"
  if ! cmp -s "$dropin_src" "$dropin_dest"; then
    log "Applying Ollama server settings"
    run sudo install -D -m 644 "$dropin_src" "$dropin_dest"
    run sudo systemctl daemon-reload
    run sudo systemctl try-restart ollama.service
  fi

  if ! systemctl is-enabled --quiet ollama.service 2>/dev/null; then
    log "Enabling ollama.service"
    run sudo systemctl enable --now ollama.service
  fi
fi
