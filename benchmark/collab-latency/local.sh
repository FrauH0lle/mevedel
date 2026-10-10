#!/usr/bin/env bash
# Run loadbot against a local relay and an isolated host of SOURCE.
#   benchmark/collab-latency/local.sh SOURCE SCENARIO [loadbot options]
# SOURCE is a byte-compiled mevedel checkout; dependencies come from this
# checkout's .eask.  Prints loadbot's JSON line, then the host stage report.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
source=$(cd "${1:?usage: local.sh SOURCE SCENARIO [options]}" && pwd)
shift
case "${1:-}" in
  edit|presence) ;;
  *) echo "local.sh supports edit and presence; prompt needs a host that drains prompts" >&2; exit 2 ;;
esac
eask=${MEVEDEL_BENCH_EASK:-$repo/.eask}
root=$(mktemp -d)
port=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1])')
relay= host=
cleanup() {
  touch "$root/stop"
  for pid in "$host" "$relay"; do
    if [ -n "$pid" ]; then
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  rm -rf "$root"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

(cd "$repo/relay" && go build -o "$root/relay" .)
"$root/relay" -addr "127.0.0.1:$port" -vapid-key-file "$root/vapid.pem" 2>"$root/relay.log" &
relay=$!
for _ in $(seq 300); do
  curl -sf --max-time 1 "http://127.0.0.1:$port/healthz" >/dev/null && break
  kill -0 "$relay" 2>/dev/null || { cat "$root/relay.log" >&2; exit 1; }
  sleep 0.1
done
curl -sf --max-time 1 "http://127.0.0.1:$port/healthz" >/dev/null || exit 1

deps=()
for d in "$eask"/*/elpa/*/; do deps+=(-L "$d"); done
(cd "$root" && exec env HOME="$root" XDG_CONFIG_HOME="$root/.config" XDG_CACHE_HOME="$root/.cache" XDG_DATA_HOME="$root/.local/share" XDG_STATE_HOME="$root/.local/state" MEVEDEL_BENCH_ROOT="$root" MEVEDEL_BENCH_RELAY="ws://127.0.0.1:$port" \
  emacs -Q --batch -L "$source" "${deps[@]}" -l "$here/local-host.el") >"$root/host.log" 2>&1 &
host=$!
for _ in $(seq 300); do [ -s "$root/links.json" ] && break; sleep 0.1; done
[ -s "$root/links.json" ] || { tail -30 "$root/host.log"; exit 1; }
full=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["full"])' "$root/links.json" |
  sed "s|^[a-z]*://[^/]*|http://127.0.0.1:$port|")

node --no-experimental-webstorage "$here/loadbot.mjs" "$full" "$@" --json
touch "$root/report"
for _ in $(seq 100); do [ -s "$root/report.eld" ] && break; sleep 0.1; done
cat "$root/report.eld"
