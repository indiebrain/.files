#!/usr/bin/env bash
#
# milano.sh — shared helpers for the milano-* command-line tools.
#
# Runs a `bin/rails runner` snippet against a live milano environment without
# adding a second Rails boot to a pod serving application traffic.
#
# Execution model — milano_query:
#   * If a shell pod is already Running (e.g. an interactive bin/remote-exec
#     session), exec the snippet into it. That pod is not ours, so there is
#     nothing to clean up and nothing that can leak.
#   * Otherwise create a run-to-completion pod from the milano-shell podtemplate
#     whose CONTAINER COMMAND IS the snippet. The pod terminates the instant the
#     query finishes — no sleep, no exec, no dependence on a client-side trap.
#     restartPolicy=Never keeps it terminal; activeDeadlineSeconds=TTL lets
#     Kubernetes force-terminate it within the TTL even if it hangs or never
#     leaves Pending. So a created pod cannot outlive min(work, TTL), enforced
#     server-side. We still delete it promptly on the way out, and reap any of
#     our own leftovers on the next run as a backstop.
#
# Output: snippets prefix their answer with RESULT_MARKER and print nothing else
# to stdout; milano_query returns just that line via the QUERY_OUTPUT global, so
# it is robust against kubectl logs merging stdout and stderr. Snippets should
# `warn` and `exit 1` to signal "not found" — milano_query then surfaces the
# message on stderr and returns non-zero.
#
# Callers override `context`, `namespace`, and `ttl` (via flag parsing) before
# calling milano_query, and read the result from QUERY_OUTPUT.

DEFAULT_CONTEXT="wc-prod-hq"
DEFAULT_NAMESPACE="milano-production"
DEFAULT_TTL="5m"
PODTEMPLATE="milano-shell"
SHELL_SELECTOR="app.kubernetes.io/component=shell"

# Prefix a snippet puts on its answer line; milano_query extracts exactly this.
# Keep in sync with the milano-* snippets that emit it.
RESULT_MARKER="__milano_result__ "

# Label stamped on pods this CLI creates, so reaping only ever removes our own
# leftovers and never an interactive remote-exec session.
MANAGED_LABEL_KEY="milano-console"
MANAGED_LABEL_VALUE="cli"
MANAGED_SELECTOR="$MANAGED_LABEL_KEY=$MANAGED_LABEL_VALUE"

context="$DEFAULT_CONTEXT"
namespace="$DEFAULT_NAMESPACE"
ttl="$DEFAULT_TTL"
ttl_seconds=""
created_pod=""
RUN_OUTPUT=""
RUN_CODE=0
QUERY_OUTPUT=""

msg() { echo >&2 "$@"; }
die() { echo "ERROR: $*" >&2; exit 1; }

# Convert a duration into whole seconds. Accepts a bare integer (seconds) or a
# single-unit value: Ns, Nm, Nh, Nd. Echoes the total; dies on anything else.
parse_duration() {
  local input="$1"
  if [[ "$input" =~ ^[0-9]+$ ]]; then
    echo "$input"
  elif [[ "$input" =~ ^([0-9]+)([smhd])$ ]]; then
    local n="${BASH_REMATCH[1]}" unit="${BASH_REMATCH[2]}"
    case "$unit" in
      s) echo "$n" ;;
      m) echo "$((n * 60))" ;;
      h) echo "$((n * 3600))" ;;
      d) echo "$((n * 86400))" ;;
    esac
  else
    die "invalid duration '$input' (use a bare number of seconds or forms like 30s, 10m, 3h, 1d)"
  fi
}

cleanup() {
  # Only delete a pod this run created; never a shell pod someone else is using.
  if [ -n "$created_pod" ]; then
    kubectl delete pod --context="$context" --namespace="$namespace" \
      --grace-period=0 --force "$created_pod" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

# Delete console pods this CLI created that are no longer Running/Pending —
# leftovers from a crashed run or an expired activeDeadlineSeconds. Scoped to
# our own label, so interactive shell pods are never touched.
reap_orphaned_pods() {
  local statuses
  statuses="$(kubectl get pod --context="$context" --namespace="$namespace" \
    --selector="$MANAGED_SELECTOR" \
    --output=jsonpath='{range .items[*]}{.metadata.name}={.status.phase};{end}' 2>/dev/null || true)"
  [ -n "$statuses" ] || return 0

  local entry name phase
  local IFS=';'
  for entry in $statuses; do
    [ -n "$entry" ] || continue
    name="${entry%%=*}"
    phase="${entry#*=}"
    case "$phase" in
      Running|Pending) ;; # in use or starting up; leave alone
      *)
        msg "Reaping orphaned console pod $name ($phase)"
        kubectl delete pod --context="$context" --namespace="$namespace" \
          --grace-period=0 --force "$name" >/dev/null 2>&1 || true
        ;;
    esac
  done
}

running_shell_pod() {
  kubectl get pod \
    --context="$context" --namespace="$namespace" \
    --field-selector="status.phase==Running" \
    --selector="$SHELL_SELECTOR" \
    --output=jsonpath="{.items[0].metadata.name}" 2>/dev/null || true
}

# Emit a run-to-completion Pod manifest derived from the podtemplate: same image,
# env, volumes, and working directory as an interactive shell pod, but with the
# container command replaced by the runner snippet (+ ARGV), restartPolicy Never,
# and activeDeadlineSeconds set to the TTL. Values are passed via the environment
# to avoid quoting the (multi-line) snippet into the manifest.
build_completion_manifest() {
  local ruby="$1"; shift
  local argv
  argv="$(printf '%s\n' "$@")"
  kubectl get podtemplate --context="$context" --namespace="$namespace" \
      "$PODTEMPLATE" --output=yaml \
    | MILANO_RUBY="$ruby" \
      MILANO_ARGV="$argv" \
      MILANO_LABEL_KEY="$MANAGED_LABEL_KEY" \
      MILANO_LABEL_VALUE="$MANAGED_LABEL_VALUE" \
      MILANO_DEADLINE="$ttl_seconds" \
      ruby -ryaml -e '
        template = YAML.parse(STDIN.read).root.to_ruby["template"]
        template = template.merge("kind" => "Pod", "apiVersion" => "v1")
        meta = (template["metadata"] ||= {})
        (meta["labels"] ||= {})[ENV["MILANO_LABEL_KEY"]] = ENV["MILANO_LABEL_VALUE"]
        argv = ENV["MILANO_ARGV"].to_s.split("\n").reject(&:empty?)
        container = template["spec"]["containers"][0]
        container["command"] = ["bin/rails", "runner", ENV["MILANO_RUBY"]] + argv
        container.delete("args")
        template["spec"]["restartPolicy"] = "Never"
        template["spec"]["activeDeadlineSeconds"] = ENV["MILANO_DEADLINE"].to_i
        puts YAML.dump(template)
      '
}

# Block until the pod reaches a terminal phase (Succeeded/Failed). Bounded a
# little past the deadline so activeDeadlineSeconds has time to fire first.
wait_terminal() {
  local pod="$1" tries=0 max phase
  max=$((ttl_seconds + 60))
  while [ "$tries" -lt "$max" ]; do
    phase="$(kubectl get pod --context="$context" --namespace="$namespace" "$pod" \
      --output=jsonpath='{.status.phase}' 2>/dev/null || true)"
    case "$phase" in
      Succeeded | Failed) return 0 ;;
    esac
    tries=$((tries + 1))
    sleep 1
  done
  die "pod $pod did not reach a terminal state within ${max}s"
}

# Run a `bin/rails runner` snippet (with optional ARGV), capturing its combined
# output in RUN_OUTPUT and the runner's exit code in RUN_CODE. This is the core
# both single-record lookups and multi-line reports build on.
#
# MUST be called in the caller's main shell (not inside $(...)), so created_pod
# and the EXIT trap survive to delete a created pod.
milano_run() {
  local ruby="$1"; shift
  command -v kubectl >/dev/null 2>&1 || die "kubectl not found on PATH"
  command -v ruby >/dev/null 2>&1 || die "ruby not found on PATH"
  ttl_seconds="$(parse_duration "$ttl")"
  RUN_OUTPUT=""
  RUN_CODE=0

  reap_orphaned_pods

  local pod
  pod="$(running_shell_pod)"
  if [ -n "$pod" ]; then
    msg "Reusing running shell pod $pod"
    RUN_OUTPUT="$(kubectl exec --context="$context" --namespace="$namespace" "$pod" \
      -- bin/rails runner "$ruby" "$@" 2>&1)" || RUN_CODE=$?
  else
    msg "Creating a run-to-completion console pod from $PODTEMPLATE (ttl ${ttl})"
    local manifest
    manifest="$(build_completion_manifest "$ruby" "$@")"
    pod="$(printf '%s' "$manifest" | kubectl create \
      --context="$context" --namespace="$namespace" \
      --output=jsonpath='{.metadata.name}' --filename -)"
    created_pod="$pod"
    wait_terminal "$pod"
    RUN_OUTPUT="$(kubectl logs --context="$context" --namespace="$namespace" "$pod" 2>&1 || true)"
    RUN_CODE="$(kubectl get pod --context="$context" --namespace="$namespace" "$pod" \
      --output=jsonpath='{.status.containerStatuses[0].state.terminated.exitCode}' 2>/dev/null || true)"
    RUN_CODE="${RUN_CODE:-1}"
  fi
}

# Run a snippet that emits a single marked answer line, and place that line
# (marker stripped) in QUERY_OUTPUT. Returns non-zero — surfacing the runner's
# output on stderr — when no answer line is produced, e.g. a "not found" exit.
# Suits record lookups; reports should call milano_run and print RUN_OUTPUT.
#
# MUST be called in the caller's main shell (see milano_run).
milano_query() {
  milano_run "$@"

  local line
  line="$(printf '%s\n' "$RUN_OUTPUT" | sed -n "s/^${RESULT_MARKER}//p" | head -n1)"
  if [ -n "$line" ]; then
    QUERY_OUTPUT="$line"
    return 0
  fi

  # No answer line: surface whatever the runner said (e.g. the not-found warn).
  [ -n "$RUN_OUTPUT" ] && printf '%s\n' "$RUN_OUTPUT" >&2
  return "${RUN_CODE:-1}"
}
