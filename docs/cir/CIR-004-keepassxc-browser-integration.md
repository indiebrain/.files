# CIR-004: Turn on KeePassXC browser integration from the installer

## Intent

Let the KeePassXC-Browser extension, which the Firefox policy file installs,
actually reach KeePassXC on a freshly installed host, and stop KeePassXC asking
which entries the extension may read on every site.

## Behavior

- GIVEN a host where the installer has run
- WHEN Firefox starts and the extension looks for KeePassXC
- THEN it finds the native messaging host and can connect to a running KeePassXC

- GIVEN KeePassXC's own settings
- WHEN its Browser Integration page is opened
- THEN browser integration is on and Firefox is ticked

- GIVEN a site the extension matches
- WHEN it asks KeePassXC for credentials
- THEN KeePassXC answers without asking which entries to expose

- GIVEN a `keepassxc.ini` holding other settings
- WHEN the installer changes these two
- THEN every other section, key, and blank line in the file is left as it was

- GIVEN KeePassXC is running while the installer runs
- WHEN the settings are written
- THEN the installer says that a running KeePassXC rewrites its configuration on
  exit and can undo them

- GIVEN the settings are already in place
- WHEN the installer runs again
- THEN nothing is written

## Constraints

- Change only the settings named here. The configuration file also carries
  per-host state, such as recently opened databases and window geometry.
- Use the same file names, locations, and contents KeePassXC itself writes, so
  that KeePassXC keeps managing them afterwards.
- Leave the host able to opt out through `overlay.conf`, like every other
  module.

## Decisions

- **Write the native messaging manifest here, rather than relying on a setting.**
  On Linux, KeePassXC has no configuration key for which browsers are enabled:
  `NativeMessageInstaller::isBrowserEnabled` reports a browser as enabled when
  its manifest file exists, and ticking the box in the settings dialog is what
  writes that file. Setting `Browser/Enabled` alone would start the service with
  no browser able to reach it.
- **Edit `keepassxc.ini` in place instead of stowing one.** A stowed file would
  make this repository the owner of every KeePassXC setting, including the list
  of recently opened databases, and would fight KeePassXC, which rewrites the
  whole file on exit. An in-place edit of two keys leaves the rest to KeePassXC.
- **`Browser/AlwaysAllowAccess` is a separate switch with its own cost.** It
  removes the per-site consent dialog entirely, so any site the extension
  matches receives its credentials silently. The extension still only offers
  entries whose URL matches the site, so the exposure is bounded by URL
  matching rather than by a human decision. It is opt-out through
  `KEEPASSXC_ALWAYS_ALLOW_ACCESS`.
- **The proxy path is resolved at install time**, from `keepassxc-proxy` on
  PATH, falling back to `/usr/bin/keepassxc-proxy`. KeePassXC derives the same
  path from its own binary's directory, so the two agree on a packaged install
  and the manifest survives KeePassXC rewriting it.
- **Passkeys stay manual.** The switch lives in the extension's own settings,
  and the extension hides it until a database is connected. Firefox can seed
  extension settings through the `3rdparty` policy, but the extension replaces
  its entire stored settings object with the managed one at every startup, which
  would discard site preferences and freeze every other extension setting to
  whatever this repository says.

## Date

2026-09-27
