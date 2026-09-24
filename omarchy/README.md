# Omarchy

The [Omarchy](https://omarchy.org) platform for these dotfiles: installs the software I use, removes the Omarchy defaults I don't, links the dotfiles with stow, and keeps it that way across `omarchy update`.

It layers on a stock Omarchy install rather than forking it. Omarchy 4 ships as pacman packages, so this drives Omarchy through its own commands (`omarchy-pkg-add`, `omarchy-remove-preinstalls`, `omarchy-mise-install`, `omarchy-default-*`, `omarchy-hook-install`) and re-applies itself from a post-update hook.

## Install

On a fresh Omarchy install, as your normal user:

```bash
git clone https://github.com/indiebrain/dotfiles.git ~/.files
~/.files/install --dry-run   # see what it would do
~/.files/install
```

The top-level `install` detects Omarchy and hands off to `omarchy/install`; macOS and Debian keep their existing path. Re-run it any time; every module is idempotent.

```
~/.files/install                      # everything
~/.files/install --only packages      # one or more modules: --only tools,dotfiles
~/.files/install --skip remove        # everything except some modules
~/.files/install --dry-run            # print actions, change nothing
~/.files/install --list               # module names in run order
```

## Layout

```
install                 top-level installer; on Omarchy: exec omarchy/install
hypr/                   stow package, Omarchy only (Hyprland bindings and window rules)
omarchy/
  install               runner: sources overlay.conf, then modules/NN-*.sh in order
  overlay.conf          settings: stow list, defaults, toggles
  lib/common.sh         helpers: log/run/dry-run, manifests, stow with backup, managed blocks
  modules/
    00-preflight.sh     checks Omarchy + non-root, bootstraps git and stow
    10-remove.sh        omarchy-remove-preinstalls (auto-confirmed) + packages/remove.packages
    20-packages.sh      packages/install.packages (pacman) + packages/aur.packages (yay)
    25-tools.sh         packages/mise.tools via omarchy-mise-install (claude, gh)
    30-dotfiles.sh      stow DOTFILES_STOW into $HOME
    40-shell.sh         managed Omarchy block in ~/.bashrc.local
    50-defaults.sh      default terminal/editor/browser
    60-emacs.sh         emacs.service user daemon
    65-ollama.sh        Ollama (GPU build auto-picked) + server settings + system service
    90-hooks.sh         post-update hook -> install --hook
  ollama/               systemd drop-in with the Ollama server settings
  packages/             one entry per line, # comments
  test/run              end-to-end test in a throwaway $HOME with Omarchy commands stubbed
```

## What gets installed

- **Repo packages** (`packages/install.packages`): stow, git-lfs, gnupg, emacs-wayland, aspell, ctags, ghostty, ttf-hack, proselint, kubectl, k9s, globalprotect-openconnect (GlobalProtect VPN), claude-desktop, firefox-developer-edition, signal-desktop, nextcloud-client.
- **CLI wrappers** (`packages/mise.tools`): Claude Code (`claude`) and GitHub CLI (`gh`). The preinstall removal deletes Omarchy's wrappers; this puts back the ones listed, on every run and after every update.
- **Ollama**: `ollama-cuda` if `nvidia-smi` exists, `ollama-rocm` if `rocminfo` exists, otherwise `ollama` (the same choice Omarchy's menu makes; pin one with `OLLAMA_PACKAGE`). Server settings live in `ollama/ollama.service.conf`, the Linux twin of the macOS LaunchAgent. The server runs as the `ollama` system user, so the `OLLAMA_*` exports in `.bashrc` don't reach it.
- **Defaults**: Ghostty (terminal), Emacs (editor), Firefox Developer Edition (browser). Omarchy's `omarchy default browser` only knows a fixed list, so Developer Edition is set with `xdg-settings`, which is what Omarchy's browser launcher reads.

## How the pieces fit

- **Stow list.** `DOTFILES_STOW` in `overlay.conf` picks the packages linked on Omarchy; iTerm2, GNOME Terminal and the ollama LaunchAgent are left out, and `hypr` is added.
- **Conflicts are backed up, not clobbered.** When Omarchy has seeded a file where a dotfile goes (`~/.bashrc`, `~/.config/ghostty/config`, `~/.config/btop/btop.conf`, `~/.config/hypr/bindings.lua`…), it is moved to `~/.local/state/dotfiles-omarchy/backups/<timestamp>/` before stowing. Stow runs with `--no-folding`, so directories stay real and files Omarchy writes into them never end up in this repo.
- **Bash.** `.bashrc` replaces Omarchy's and already sources `~/.bashrc.local`; the shell module keeps a marked block there that sources Omarchy's `env-bootstrap` (OMARCHY_PATH, `omarchy-*` on PATH, mise shims). Set `OMARCHY_BASH=full` to also load Omarchy's aliases, functions and prompt tools. Lines outside the block are yours.
- **Removals follow upstream.** The remove module calls Omarchy's own `omarchy-remove-preinstalls` (with a `gum` shim that answers its confirm prompt), so when upstream adds or drops a preinstall, this follows. Extra removals go in `packages/remove.packages`.
- **Updates.** `omarchy update` runs `~/.config/omarchy/hooks/post-update.d/50-dotfiles-omarchy`, which re-runs the `preflight,remove,packages,tools,dotfiles,shell` modules. It does not `git pull` this repo; update it yourself.

## GlobalProtect VPN

`globalprotect-openconnect` is the open-source GlobalProtect client, from Arch's extra repo. Connect with SAML SSO in your default browser (Firefox Developer Edition):

```bash
gpclient connect --browser default vpn.example.com
```

The CLI is free; its GUI is paid after a 7-day trial. If your VPN insists on Palo Alto's official client (e.g. strict HIP checks), that client ships as a `PanGPLinux-*.tgz` from your company's support portal; package it with the AUR `globalprotect-bin` recipe.

## Per-host overrides

Create `~/.config/dotfiles-omarchy/overlay.conf` with any variables from `overlay.conf`, e.g.

```bash
EMACS_DAEMON=false
OLLAMA=false
```

## Adding things

- A package: add a line to `packages/install.packages` (or `aur.packages`), run `install --only packages`.
- A mise-backed CLI: add it to `packages/mise.tools`, run `install --only tools`.
- A dotfile: add the stow package at the repo root and its name to `DOTFILES_STOW`.
- A new step: drop `modules/NN-name.sh` in; it's picked up automatically. Use `run` for anything that changes the system so `--dry-run` works, and add it to `HOOK_MODULES` in `install` if it should re-run after updates.

## Known interactions

- **Ghostty keybindings.** The Ghostty config binds many `super+…` keys (macOS Cmd habits: `super+w`, `super+t`, `super+1..9`, `super+enter`, `super+f`, `super+c/v`…). Hyprland grabs most `SUPER` combos first, so those binds won't fire on Omarchy. Consider a `config-file = ?linux.conf` include with `ctrl+shift` variants, or rebinding in `hypr/.config/hypr/bindings.lua`.
- **Themes.** Stowed Ghostty and btop configs use the Modus themes instead of following `omarchy theme` switches.
- **Tool versions.** Omarchy uses mise; mise reads `.tool-versions` and `~/.default-gems`, so asdf itself isn't installed.
- **Ollama sizing.** The server settings were sized on the 128 GB Mac; on smaller machines lower `OLLAMA_MAX_LOADED_MODELS` and `OLLAMA_CONTEXT_LENGTH`.

## Test

```bash
omarchy/test/run
```
