# CIR-003: Configure Firefox Developer Edition from the installer

## Intent

Have a freshly installed host come up with Firefox configured the way it is
meant to be used, rather than leaving a list of manual clicks through
`about:preferences`: the two extensions that carry the browsing setup, DuckDuckGo
as the search engine with nothing else on offer, no offers to store credentials
or payment details, and no telemetry or suggested content.

## Behavior

- GIVEN a host where the installer has run
- WHEN Firefox Developer Edition starts
- THEN Privacy Badger and KeePassXC-Browser are installed and cannot be removed

- GIVEN a search from the address bar or search bar
- WHEN no engine is chosen explicitly
- THEN DuckDuckGo serves it, the built-in alternatives are hidden, and a webpage
  cannot add an engine

- GIVEN a login form, a payment form, or an address form
- WHEN it is submitted
- THEN Firefox does not offer to save the credentials, card, or address, and does
  not record the form history

- GIVEN a new tab or an address bar suggestion
- WHEN it is displayed
- THEN it carries no sponsored content, stories, or recommendation messages, and
  Firefox sends no telemetry or study data

- GIVEN a host where Firefox is already running
- WHEN the installer applies the settings
- THEN the settings take effect at the next Firefox start, and the installer says
  so

- GIVEN a macOS host
- WHEN `scripts/bin/firefox-apply-policies` runs
- THEN the same settings reach Firefox Developer Edition there, and running it
  again reports that they are already current

- GIVEN a macOS host where Firefox has updated itself since
- WHEN the script runs again
- THEN the settings are restored, without needing the rest of the host setup

- GIVEN a host that wants Firefox left alone
- WHEN `FIREFOX_POLICIES=false` is set in a per-host override
- THEN the installer changes nothing about Firefox

- GIVEN the installer runs again with the settings already in place
- WHEN the module compares them
- THEN nothing is written and nothing is restarted

## Constraints

- Configure the browser the packaged Developer Edition actually is, without
  patching the package or pinning a profile by name.
- Keep the settings in one reviewable file in this repository, the way
  `omarchy/ollama/ollama.service.conf` holds the Ollama settings.
- Leave the host able to opt out, through the same `overlay.conf` mechanism as
  every other module.

## Decisions

- **An enterprise policy file rather than preferences in the profile.** A
  `user.js` would reach some of the same settings, but a profile lives at
  `~/.mozilla/firefox/<random>.dev-edition-default`, and the random component
  means no stow package or fixed path can place a file inside it. Policies also
  reach what preferences cannot: installing an extension, and hiding a built-in
  search engine.
- **The `distribution` directory beside the binary, not
  `/etc/firefox/policies`.** Firefox only reads the system-wide path in a build
  compiled with system policies enabled. The packaged Developer Edition is
  Mozilla's own build, which is not, so the system path would leave every policy
  silently unapplied. The `distribution` directory is read through the
  application directory in all builds, and the package already ships one.
- **One policy file for both platforms, at the repository root.** The file is
  platform-neutral, so it lives in the `firefox` stow package rather than under
  the Omarchy overlay, listed in that package's `.stow-local-ignore` so stow
  does not link it into the home directory. Each platform's installer copies it
  where that platform's Firefox reads it.
- **On macOS, the application bundle rather than a configuration profile.** The
  documented macOS route is a configuration profile, but installing one without
  device management means a person approving it in System Settings, which an
  installer cannot do. `scripts/bin/firefox-apply-policies` writes
  `Contents/Resources/distribution/policies.json` inside the bundle instead.
  The cost is durability: a Firefox update replaces the bundle and takes the
  file with it, so the script is written to be re-run and says so.
- **Search engine policy is viable on this channel.** The `SearchEngines` group
  was restricted to the Extended Support Release for years, which would have
  ruled out the search half of this. Firefox 139 opened it to every channel, and
  the packaged Developer Edition is well past that.
- **Extensions are `force_installed`, not `normal_installed`.** The stronger mode
  installs them without a prompt and stops them being removed, which is what
  "these are part of the setup" means. Relax it to `normal_installed` for an
  extension that should be removable.
- **`Remove` hides engines by display name.** This is the only lever the policy
  offers; there is no "remove everything else". An engine that a locale or region
  ships under a name not in the list survives, so the list is a known-incomplete
  defense that `about:preferences#search` can correct.
- **The module refuses a policy file that is not valid JSON.** Firefox reacts to
  a malformed file by ignoring every policy in it and reporting the problem only
  in `about:policies`. Validating before installing turns that into a failure at
  install time, where it is visible.
- **KeePassXC's own browser integration stays manual.** The extension talks to
  KeePassXC over a native messaging host that KeePassXC registers when its
  browser integration setting is enabled, which lives in the application's own
  configuration rather than anything Firefox can be told.

## Date

2026-09-27
