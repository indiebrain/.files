# CIR-002: Install the Emacs typeface from the platform installers

## Intent

Make a freshly installed machine render the Emacs configuration as written. The
fontaine presets in `emacs/.emacs.d/indiebrain-emacs-modules/indiebrain-emacs-font.el`
select faces by family name. When those families are absent, Emacs quietly
substitutes something else and the configured typography is lost, with no error
to explain it. Neither the Omarchy installer nor the macOS host setup installed
them, so the families reached a machine only by hand.

## Behavior

- GIVEN a fresh Omarchy machine
- WHEN `~/.files/install` runs
- THEN the families the fontaine presets name are installed from the AUR, and
  Emacs renders the presets as written

- GIVEN a fresh macOS machine
- WHEN `scripts/bin/initial-host-setup` runs
- THEN the same families are installed through Homebrew

- GIVEN the fonts are already installed
- WHEN either installer runs again
- THEN the package is recognized as present and nothing is reinstalled

- GIVEN Ghostty, which selects Hack
- WHEN the installers run
- THEN its font selection is unaffected

- GIVEN the fonts placed by hand in `~/Library/Fonts` on an existing machine
- WHEN the macOS setup runs
- THEN they are left in place; removing them is the owner's choice

## Constraints

- Drive Omarchy through its own helpers and existing manifests, so a font is one
  line in `omarchy/packages/aur.packages` rather than a module of its own.
- On macOS, install the fonts where the rest of the software already comes from,
  `scripts/bin/initial-host-setup`, not from the top-level `install` script,
  which installs only the dotfiles plumbing.
- Prefer a packaged font over a downloader in this repository, so updates arrive
  through the package manager.

## Decisions

- **Moved from Iosevka Comfy to Aporetic.** Iosevka Comfy is discontinued
  upstream, because "Iosevka" is a reserved name. Its Homebrew cask,
  `font-iosevka-comfy`, was disabled on 2026-02-22 and names `font-aporetic` as
  the replacement, so no cask can install it. Aporetic is the same author's
  successor build of Iosevka and is packaged on both platforms, `ttf-aporetic`
  on the AUR and `font-aporetic` on Homebrew, both tracking upstream release
  1.2.0.
- **Rejected: keep Iosevka Comfy and install it on macOS from the upstream
  release archive.** It would work today, since the AUR recipe still builds and
  the archives still download. It puts a hand-written downloader in this
  repository to serve a typeface that will receive no further releases, and
  leaves the two platforms installing different versions, 2.0.0 from the AUR
  against 2.1.0 from the archive.
- **Rejected: install from the `indiebrain/iosevka-comfy` copy.** That
  repository carries no tags and no releases, so an installer has nothing to
  pin and would track a branch.
- **The family mapping preserves the previous pairing.** The default face keeps
  a sans monospace, `Aporetic Sans Mono`, and variable pitch keeps a slab
  quasi-proportional, `Aporetic Serif`, with the `small` preset keeping its
  inversion of the two. Aporetic's serif families are slab builds, the same
  shapes the Iosevka Comfy Motion variants carried, so the contrast between
  fixed and variable pitch reads as it did.
- **Dropped the weight overrides.** Aporetic ships regular and bold, upright and
  slanted. The `medium` preset asked for a semilight default with extrabold
  bold, and `presentation` asked for light. None of those weights exist, and a
  preset that names a missing weight silently resolves to whatever is nearest.
  The presets now vary by size alone, and `medium` is 130 against the fallback
  120 so that it still differs from `regular`.

## Date

2026-09-27
