#!/bin/sh
# Run loadbot against a local relay and an isolated host of SOURCE.
#   benchmark/collab-latency/local.sh SOURCE SCENARIO [loadbot options]
# SOURCE is a byte-compiled mevedel checkout; dependencies come from this
# checkout's .eask.  Prints loadbot's JSON line, then the host stage report.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
source=$(cd "${1:?usage: local.sh SOURCE SCENARIO [options]}" && pwd); shift
eask=${MEVEDEL_BENCH_EASK:-$(git -C "$repo" worktree list | head -1 | cut -d' ' -f1)/.eask}
root=$(mktemp -d)
port=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1])')
trap 'touch "$root/stop"; kill $relay 2>/dev/null; sleep 0.5; rm -rf "$root"' EXIT

(cd "$repo/relay" && go build -o "$root/relay" .)
"$root/relay" -addr "127.0.0.1:$port" -vapid-key-file "$root/vapid.pem" 2>"$root/relay.log" &
relay=$!
until curl -sf "http://127.0.0.1:$port/healthz" >/dev/null; do sleep 0.1; done

deps=$(for d in "$eask"/*/elpa/*/; do printf -- '-L %s ' "$d"; done)
(cd "$root" && HOME="$root" MEVEDEL_BENCH_ROOT="$root" MEVEDEL_BENCH_RELAY="ws://127.0.0.1:$port" \
  emacs -Q --batch -L "$source" $deps -l "$here/local-host.el" >"$root/host.log" 2>&1 &)
for _ in $(seq 300); do [ -s "$root/links.json" ] && break; sleep 0.1; done
[ -s "$root/links.json" ] || { tail -30 "$root/host.log"; exit 1; }
full=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["full"])' "$root/links.json" |
  sed "s|^[a-z]*://[^/]*|http://127.0.0.1:$port|")

node --no-warnings "$here/loadbot.mjs" "$full" "$@" --json
touch "$root/report"
for _ in $(seq 100); do [ -s "$root/report.eld" ] && break; sleep 0.1; done
cat "$root/report.eld"
