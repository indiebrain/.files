# Shared helpers for dotfiles-omarchy modules. Sourced, not executed.
# shellcheck shell=bash

STATE_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-omarchy"
BACKUP_DIR="$STATE_DIR/backups/$(date +%Y%m%d-%H%M%S)"
DRY_RUN="${DRY_RUN:-0}"

if [[ -t 1 ]]; then
  _c_blue=$'\e[34m' _c_yellow=$'\e[33m' _c_red=$'\e[31m' _c_dim=$'\e[2m' _c_reset=$'\e[0m'
else
  _c_blue="" _c_yellow="" _c_red="" _c_dim="" _c_reset=""
fi

log() { printf '%s==>%s %s\n' "$_c_blue" "$_c_reset" "$*"; }
warn() { printf '%s!!%s  %s\n' "$_c_yellow" "$_c_reset" "$*" >&2; }
die() {
  printf '%sxx%s  %s\n' "$_c_red" "$_c_reset" "$*" >&2
  exit 1
}

# Run a command, or print it when DRY_RUN=1.
run() {
  if (( DRY_RUN )); then
    printf '%s    [dry-run] %s%s\n' "$_c_dim" "$*" "$_c_reset"
  else
    "$@"
  fi
}

# Print non-comment, non-blank lines of a manifest (trailing comments allowed).
read_manifest() {
  local file="$1" line
  [[ -f $file ]] || return 0
  while IFS= read -r line || [[ -n $line ]]; do
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -n $line ]] && printf '%s\n' "$line"
  done <"$file"
}

# Move a file out of the way into this run's backup directory, keeping its path.
backup_path() {
  local target="$1" rel dest
  rel="${target#"$HOME"/}"
  dest="$BACKUP_DIR/$rel"
  log "Backing up ~/$rel -> ${dest/#$HOME/\~}"
  run mkdir -p "$(dirname "$dest")"
  run mv -- "$target" "$dest"
}

# Stow packages from <dir> into $HOME. Any real file that stow would refuse to
# replace (e.g. a default Omarchy seeded into ~/.config) is backed up first.
# --no-folding links individual files, so directories like ~/.config/ghostty
# stay real and anything Omarchy writes there never lands in your repo.
stow_packages() {
  local dir="$1"
  shift
  local pkg conflict
  for pkg in "$@"; do
    if [[ ! -d $dir/$pkg ]]; then
      warn "stow package '$pkg' not found in $dir, skipping"
      continue
    fi
    while IFS= read -r conflict; do
      [[ -n $conflict ]] && backup_path "$HOME/$conflict"
    done < <(stow_conflicts "$dir" "$pkg")
    log "Stowing $pkg"
    run stow --no-folding --restow --dir "$dir" --target "$HOME" "$pkg"
  done
}

# List paths (relative to $HOME) that block stowing <pkg>. Understands the
# conflict wording of stow 2.3 and 2.4.
stow_conflicts() {
  local dir="$1" pkg="$2"
  stow --no --verbose=1 --no-folding --restow --dir "$dir" --target "$HOME" "$pkg" 2>&1 |
    sed -nE \
      -e 's/^ *\* existing target is neither a link nor a directory: (.*)$/\1/p' \
      -e 's/^ *\* existing target is not owned by stow: (.*)$/\1/p' \
      -e 's/^ *\* cannot stow .* over existing target (.*) since neither a link nor a directory.*$/\1/p' \
      -e 's/^ *\* existing target (.*) is not owned by stow.*$/\1/p' |
    sort -u
}

# Keep a named, marker-delimited block in <file> in sync with stdin.
# Anything outside the markers is left alone.
ensure_block() {
  local file="$1" name="$2" begin end content tmp
  begin="# >>> $name (managed by dotfiles-omarchy) >>>"
  end="# <<< $name <<<"
  content="$(cat)"
  tmp="$(mktemp)"
  if [[ -f $file ]]; then
    awk -v b="$begin" -v e="$end" '
      $0 == b { skip = 1; next }
      $0 == e { skip = 0; next }
      !skip { print }
    ' "$file" >"$tmp"
  fi
  # Drop trailing blank lines, then append the fresh block.
  sed -i -e ':a' -e '/^\n*$/{$d;N;ba' -e '}' "$tmp"
  [[ -s $tmp ]] && printf '\n' >>"$tmp"
  printf '%s\n%s\n%s\n' "$begin" "$content" "$end" >>"$tmp"
  if [[ -f $file ]] && cmp -s "$tmp" "$file"; then
    rm -f "$tmp"
    return 0
  fi
  log "Updating managed block '$name' in ${file/#$HOME/\~}"
  if (( DRY_RUN )); then
    rm -f "$tmp"
  else
    mkdir -p "$(dirname "$file")"
    mv "$tmp" "$file"
  fi
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "'$1' not found; is this an Omarchy system?"
}
