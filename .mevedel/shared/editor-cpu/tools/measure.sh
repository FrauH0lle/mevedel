#!/bin/bash
# measure.sh OUT-DIR -- measure mevedel animation and wakeup costs in the live Emacs.
#
# Requires: the Emacs under test reachable with `emacsclient', its frame on
# screen and uncovered (an occluded or locked screen skips the repaint and
# understates every number), no stray demo timers, and nobody typing in it.
# Starts the mock server, loads cpuh-harness.el, runs every scenario for 10 s,
# then restores the user's settings and unloads the harness.  QUICK=1 stops
# after the first shimmer scenario, as a smoke test.
set -u
K=$(cd "$(dirname "$0")" && pwd); OUT=${1:?output directory}; mkdir -p "$OUT"
RES=$OUT/results.txt; TIM=$OUT/timers.txt; : > "$RES"; : > "$TIM"
CTL=$OUT/control.json; echo '{"hold": 0, "tool": null}' > "$CTL"
WS=$(mktemp -d); (cd "$WS" && git init -q && echo "# cpu" > README.md && git add -A && git -c user.email=t@t -c user.name=t commit -qm init)
python3 -I "$K/mock_server.py" 8766 "$CTL" "$OUT/body.json" > "$OUT/mock.log" 2>&1 & MOCK=$!
trap 'kill $MOCK 2>/dev/null; emacsclient --eval "(when (fboundp (quote cpuh-unload)) (cpuh-unload))" >/dev/null; rm -rf "$WS"' EXIT
EPID=$(emacsclient --eval '(emacs-pid)'); KPID=$(pgrep -x kwin_wayland || pgrep -x gnome-shell || echo 1)
cpu() { awk '{print $14+$15}' /proc/$1/stat; }
emacsclient --eval "(progn (load \"$K/cpuh-harness.el\" nil t) t)" >/dev/null
emacsclient --eval "(cpuh-session-string \"$WS\" 'cpu-mock)" >/dev/null
rearm() { emacsclient --eval '(with-current-buffer (cpuh-view) (mevedel-view--start-spinner-timer t) t)' >/dev/null; }
sample() { # LABEL: 10 s of emacs/compositor CPU and timer callbacks
  [ "${REARM:-1}" = 1 ] && rearm
  emacsclient --eval '(cpuh-start)' >/dev/null
  local e0=$(cpu $EPID) k0=$(cpu $KPID); sleep 10; local e1=$(cpu $EPID) k1=$(cpu $KPID)
  emacsclient --eval "(cpuh-dump \"$TIM\" \"$1\")" >/dev/null; emacsclient --eval '(cpuh-stop)' >/dev/null
  local t=$(awk -v l="== $1 " 'index($0,l)==1{f=1;next} /^==/{f=0} f{s+=$1} END{print s+0}' "$TIM")
  awk -v l="$1" -v e=$((e1-e0)) -v k=$((k1-k0)) -v t=$t 'BEGIN{printf "%-52s emacs %5.1f%%  compositor %5.1f%%  timers/s %5.1f\n", l, e/10, k/10, t/10}' | tee -a "$RES"
}
wait_idle() { for i in $(seq 1 60); do [ "$(emacsclient --eval '(cpuh-busy-p)')" = nil ] && return; sleep 1; done; }
scenario() { # LABEL STYLE TOOL-STYLE FPS TELEMETRY HOLD TOOL-JSON
  emacsclient --eval "(progn (mevedel-telemetry--lag-stop) (cpuh-set '$2 '$3 $4 $5))" >/dev/null
  echo "{\"hold\": $6, \"tool\": $7}" > "$CTL"
  emacsclient --eval '(cpuh-send "measure")' >/dev/null
  sleep 4; sample "$1"; wait_idle; sleep 2
}
emacsclient --eval '(progn (mevedel-telemetry--lag-stop) (cpuh-attend t))' >/dev/null
sleep 2; sample "idle (no request)"
BASH='{"name": "Bash", "args": {"command": "sleep 16", "yield_time_ms": 30000}}'
scenario "label static"                  static   static  30 nil 16 null
scenario "label shimmer @30"             shimmer  static  30 nil 16 null
[ "${QUICK:-0}" = 1 ] && exit 0
scenario "label shimmer @15"             shimmer  static  15 nil 16 null
scenario "label breathe @30"             breathe  static  30 nil 16 null
scenario "label bounce @30"              bounce   static  30 nil 16 null
scenario "label dots"                    dots     static  30 nil 16 null
scenario "label ellipsis"                ellipsis static  30 nil 16 null
scenario "label braille"                 braille  static  30 nil 16 null
scenario "label ascii"                   ascii    static  30 nil 16 null
scenario "tool static (label static)"    static   static  30 nil 0 "$BASH"
scenario "tool shimmer (label static)"   static   shimmer 30 nil 0 "$BASH"
scenario "tool braille (label static)"   static   braille 30 nil 0 "$BASH"
scenario "heartbeat (label static)"      static   static  30 t   16 null
scenario "defaults: shimmer + shimmer tool + heartbeat, Bash" shimmer shimmer 30 t 0 "$BASH"
scenario "defaults on battery (15 fps), Bash" shimmer shimmer 15 t 0 "$BASH"
