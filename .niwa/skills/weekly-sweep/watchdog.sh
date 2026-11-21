#!/usr/bin/env bash
# Resource watchdog for the weekly sweep, run as a Monitor outside confinement.
# Usage: watchdog.sh EMACS_PID [INTERVAL_SECONDS] [--heartbeat MINUTES]
# Prints a line only when the state changes (the first OK included), so a
# healthy run stays silent and wakes nobody.  With --heartbeat it also prints
# HEARTBEAT every MINUTES; use that only during a planned pause, to keep the
# coordinator's one-hour prompt cache warm (see BUDGET.md).
# Under memory pressure it kills processes started with `choom -n 1000`
# (test workers), never Emacs.  Every line is also appended to a log for
# post-mortems, since a stalled Emacs cannot read Monitor events.
set -u
pid=${1:?usage: watchdog.sh EMACS_PID [INTERVAL] [--heartbeat MINUTES]}
shift
interval=30 heartbeat=0
while [ $# -gt 0 ]; do
  case $1 in
    --heartbeat) heartbeat=$((${2:?--heartbeat needs MINUTES} * 60)); shift 2 ;;
    *) interval=$1; shift ;;
  esac
done
log=${XDG_CACHE_HOME:-$HOME/.cache}/niwa-sweep-watchdog.log
warn_kb=$((8 * 1024 * 1024))
kill_kb=$((4 * 1024 * 1024))
rss_warn_kb=$((12 * 1024 * 1024))
mkdir -p "$(dirname "$log")"

emit() {
  local line
  line="$(date +%T) $*"
  printf '%s\n' "$line"
  printf '%s\n' "$line" >>"$log"
}

kill_test_workers() {
  local n=0 f p
  for f in /proc/[0-9]*/oom_score_adj; do
    p=${f#/proc/}; p=${p%%/*}
    [ "$(cat "$f" 2>/dev/null)" = 1000 ] || continue
    [ -O "/proc/$p" ] || continue
    kill -TERM "$p" 2>/dev/null && n=$((n + 1))
  done
  echo "$n"
}

state="" last_beat=$(date +%s)
while kill -0 "$pid" 2>/dev/null; do
  avail=$(awk '/^MemAvailable:/ {print $2}' /proc/meminfo)
  rss=$(awk '/^VmRSS:/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo 0)
  ping=ok
  if command -v emacsclient >/dev/null &&
      ! timeout 20 emacsclient --eval t >/dev/null 2>&1; then
    ping=unresponsive
  fi
  new=OK
  if [ "$avail" -lt "$kill_kb" ]; then
    new="PRESSURE killed=$(kill_test_workers)"
  elif [ "$avail" -lt "$warn_kb" ] || [ "$rss" -gt "$rss_warn_kb" ]; then
    new=PRESSURE
  elif [ "$ping" != ok ]; then
    new=STALL
  fi
  now=$(date +%s)
  if [ "${new%% *}" != "${state%% *}" ] || [ "${new#PRESSURE killed=}" != "$new" ]; then
    emit "$new avail_mb=$((avail / 1024)) emacs_rss_mb=$((rss / 1024)) ping=$ping"
  fi
  if [ "$heartbeat" -gt 0 ] && [ $((now - last_beat)) -ge "$heartbeat" ]; then
    emit "HEARTBEAT keep the prompt cache warm; reply in one word and end the turn"
    last_beat=$now
  fi
  state=$new
  sleep "$interval"
done
emit "EXIT emacs pid $pid gone"
