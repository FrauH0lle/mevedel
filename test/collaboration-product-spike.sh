#!/usr/bin/env bash
set -eu

spike_root=$(cd "$(dirname "$0")/.." && pwd)
spike_tmp=$(mktemp -d /tmp/mevedel-collaboration-product-spike.XXXXXX)
state_file="$spike_tmp/state"
ready_file="$spike_tmp/ready"
stop_file="$spike_tmp/stop"
burst_file="$spike_tmp/burst"
lifecycle_file="$spike_tmp/lifecycle"
settle_file="$spike_tmp/settle"
mutation_file="$spike_tmp/mutation"
mutation_state_file="$spike_tmp/mutation-state"
log_file="$spike_tmp/emacs.log"
client_log="$spike_tmp/client.log"
emacs_pid=''
node_pid=''

cleanup() {
  if [ -n "$node_pid" ] && kill -0 "$node_pid" 2>/dev/null; then
    kill "$node_pid" 2>/dev/null || true
    wait "$node_pid" 2>/dev/null || true
  fi
  if [ -n "$emacs_pid" ] && kill -0 "$emacs_pid" 2>/dev/null; then
    touch "$stop_file"
    wait "$emacs_pid" 2>/dev/null || true
  fi
  rm -rf "$spike_tmp"
}
trap cleanup EXIT

web_server_dir=${MEVEDEL_WEB_SERVER_DIR:-}
if [ -z "$web_server_dir" ]; then
  for candidate in \
    "$spike_root/.eask/30.2/elpa/web-server-0.1.2"; do
    if [ -f "$candidate/web-server.el" ]; then
      web_server_dir=$candidate
      break
    fi
  done
fi
if [ -z "$web_server_dir" ] || [ ! -f "$web_server_dir/web-server.el" ]; then
  echo "GNU ELPA web-server 0.1.2 is unavailable; set MEVEDEL_WEB_SERVER_DIR" >&2
  exit 2
fi

gptel_dir=${MEVEDEL_GPTEL_DIR:-}
if [ -z "$gptel_dir" ]; then
  for candidate in \
    "$spike_root/.eask/30.2/elpa"/gptel-*; do
    if [ -f "$candidate/gptel.el" ]; then
      gptel_dir=$candidate
      break
    fi
  done
fi
if [ -z "$gptel_dir" ] || [ ! -f "$gptel_dir/gptel.el" ]; then
  echo "gptel is unavailable; set MEVEDEL_GPTEL_DIR" >&2
  exit 2
fi

MEVEDEL_SPIKE_STATE="$state_file" \
MEVEDEL_SPIKE_STOP="$stop_file" \
MEVEDEL_SPIKE_BURST="$burst_file" \
MEVEDEL_SPIKE_LIFECYCLE="$lifecycle_file" \
MEVEDEL_SPIKE_SETTLE="$settle_file" \
MEVEDEL_SPIKE_MUTATION="$mutation_file" \
MEVEDEL_SPIKE_MUTATION_STATE="$mutation_state_file" \
  emacs --batch -Q -L "$web_server_dir" -L "$gptel_dir" -L "$spike_root" \
  -l "$spike_root/test/collaboration-product-spike.el" \
  >"$log_file" 2>&1 &
emacs_pid=$!

for _ in $(seq 1 100); do
  if [ -s "$state_file" ]; then break; fi
  if ! kill -0 "$emacs_pid" 2>/dev/null; then
    sed -n '1,160p' "$log_file" >&2
    exit 1
  fi
  sleep 0.05
done
[ -s "$state_file" ] || { sed -n '1,160p' "$log_file" >&2; exit 1; }

IFS=: read -r state host port room token < "$state_file"
[ "$state" = started ] || { cat "$log_file" >&2; exit 1; }
MEVEDEL_SPIKE_READY="$ready_file" \
MEVEDEL_SPIKE_BURST="$burst_file" \
MEVEDEL_SPIKE_LIFECYCLE="$lifecycle_file" \
MEVEDEL_SPIKE_SETTLE="$settle_file" \
MEVEDEL_SPIKE_MUTATION="$mutation_file" \
MEVEDEL_SPIKE_MUTATION_STATE="$mutation_state_file" \
  node "$spike_root/test/collaboration-product-client.js" \
    "$host" "$port" "$room" "$token" >"$client_log" 2>&1 &
node_pid=$!

for _ in $(seq 1 600); do
  if [ -s "$ready_file" ]; then break; fi
  if ! kill -0 "$node_pid" 2>/dev/null; then
    sed -n '1,240p' "$client_log" >&2
    exit 1
  fi
  sleep 0.05
done
[ -s "$ready_file" ] || { sed -n '1,240p' "$client_log" >&2; exit 1; }
touch "$stop_file"
if ! wait "$node_pid"; then
  sed -n '1,240p' "$client_log" >&2
  exit 1
fi
node_pid=''

if kill -0 "$emacs_pid" 2>/dev/null; then
  kill -INT "$emacs_pid" 2>/dev/null || true
  wait "$emacs_pid" 2>/dev/null || true
fi
emacs_pid=''

if curl --silent --show-error --max-time 1 \
  "http://$host:$port/index.html" >/dev/null 2>&1; then
  echo "product collaboration teardown failed: listener is still reachable" >&2
  exit 1
fi
echo "product collaboration loopback passed"
