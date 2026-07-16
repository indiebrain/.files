# CIR-001: Scope milano review-stack leaked-namespace sweep to a cluster tier

## Intent

Give the `milano review-stack list-leaked-namespaces` command a way to report
only the vestigial Kubernetes review-environment namespaces in a specific
cluster tier — in practice the beta tier, which is the only tier Review Stacks
are ever provisioned into. The goal is a focused cleanup list: namespaces whose
owning `Shipit::ReviewStack` is deprovisioned or whose stack record is gone
entirely, without noise from clusters that can never hold a review namespace.

## Behavior

- GIVEN the beta-tier clusters hold review-stack namespaces whose stacks are
  deprovisioned or whose stack records were deleted
- WHEN I run `milano review-stack list-leaked-namespaces --tier beta`
- THEN only those namespaces are reported, each with a REASON of
  "stack deprovisioned" or "stack record deleted"

- GIVEN one or more `--tier` values
- WHEN the sweep runs
- THEN only clusters in those tiers (via `Cluster.in_tiers`) are inspected, and
  `--tier` is repeatable so several tiers can be combined

- GIVEN a `--tier` value that matches no configured cluster (a typo or a dead
  tier)
- WHEN the sweep runs
- THEN the command exits non-zero and prints the known tiers, rather than
  reporting a false all-clear

- GIVEN no `--tier` option
- WHEN the sweep runs
- THEN behavior is unchanged: every configured cluster is inspected (the
  pre-existing default is preserved)

- GIVEN `--tier` is passed to any action other than `list-leaked-namespaces`
  (for example `list-stuck`)
- WHEN the command is parsed
- THEN it fails loudly with an error rather than silently ignoring the flag

## Constraints

- Reuse the existing, proven leak-detection snippet in `milano-review-stack`
  (namespace label sweep → match to `Shipit::ReviewStack` by stack-id label →
  flag deprovisioned or missing) rather than reimplementing it.
- Depend only on durable milano internals already used by the file: the
  `Cluster` library (`Cluster.in_tiers`, `Cluster.all`) and
  `KubernetesClients` — no new app-side classes.
- Follow the file's established flag-parsing and ARGV-passing patterns; tiers
  travel to the runner snippet as ARGV, consistent with how the other snippets
  receive arguments.

## Decisions

- **Extend the existing command with a `--tier` filter, rather than add a new
  purpose-built subcommand.** A dedicated `list-vestigial-beta` action was
  considered and rejected: it would duplicate almost the entire runner snippet,
  and the two copies would drift out of sync. `--tier` generalizes the existing
  command instead. This was steered here deliberately — the initial framing was
  "write a new tool," but a near-identical command already existed, so the
  smaller, non-duplicative change was chosen.
- **Report both "stack deprovisioned" and "stack record deleted", not
  deprovisioned only.** A strict reading of the request was deprovisioned-only,
  but both classes are vestigial namespaces worth cleaning up, and the existing
  REASON column already distinguishes them. Narrowing to deprovisioned-only
  would have hidden the larger orphaned-record population (35 of 36 in the first
  live beta sweep).
- **Default remains all clusters.** Keeping the tier filter opt-in preserves the
  command's existing behavior for any current callers; `--tier beta` opts into
  the narrower, faster sweep.
- **A tier matching zero clusters is a hard error, not an empty success.** A
  silent "No leaked namespaces found" on a mistyped tier would read as an
  all-clear and could hide real leaks, so the snippet exits non-zero and lists
  the known tiers.

## Date

2026-07-16
