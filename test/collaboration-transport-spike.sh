#!/usr/bin/env bash
set -eu

spike_root=$(cd "$(dirname "$0")/.." && pwd)
spike_tmp=$(mktemp -d /tmp/mevedel-collaboration-spike.XXXXXX)
state_file="$spike_tmp/state"
stop_file="$spike_tmp/stop"
log_file="$spike_tmp/emacs.log"
emacs_pid=''

cleanup() {
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

MEVEDEL_SPIKE_ROOT="$spike_root" \
MEVEDEL_SPIKE_STATE="$state_file" \
MEVEDEL_SPIKE_STOP="$stop_file" \
  emacs --batch -Q -L "$web_server_dir" \
  -l "$spike_root/test/collaboration-transport-spike.el" \
  >"$log_file" 2>&1 &
emacs_pid=$!

for _ in $(seq 1 100); do
  if [ -s "$state_file" ]; then break; fi
  if ! kill -0 "$emacs_pid" 2>/dev/null; then
    sed -n '1,120p' "$log_file" >&2
    exit 1
  fi
  sleep 0.05
done
[ -s "$state_file" ] || { sed -n '1,120p' "$log_file" >&2; exit 1; }

IFS=: read -r state host port < "$state_file"
[ "$state" = started ] || { cat "$log_file" >&2; exit 1; }
node "$spike_root/test/collaboration-transport-client.js" "$host" "$port"

touch "$stop_file"
wait "$emacs_pid" 2>/dev/null || true
emacs_pid=''

if curl --silent --show-error --max-time 1 "http://$host:$port/index.html" >/dev/null 2>&1; then
  echo "transport spike teardown failed: listener is still reachable" >&2
  exit 1
fi
echo "transport spike teardown passed"
