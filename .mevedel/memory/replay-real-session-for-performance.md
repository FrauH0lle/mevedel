---
name: "Replay performance stalls from a restored copy of the real session"
description: "A transcript pasted into detached test buffers skips agent transcripts, archived segments and publications, so it can miss the stall entirely"
type: project
---

# Replay performance stalls from a restored copy of the real session

On 2026-10-03 the first fix for session `2026-10-03T07-07-aa7dca1083e9`
(multi-second `mevedel-view--flush-scheduled-render` stalls) was validated by a
replay that inserted the saved transcript into `mevedel-view-test--with-buffers`.
Those buffers have no session, so nothing reached agent transcripts, archived
segments or the session's published artifacts. The replay reported
5.9 ms updates while the real code paths still cost 0.36 s per streamed update.

A second replay restored a copy of the real session (read-only; the live Emacs
kept its lease) and replayed the request that froze: three agents running, a
700 KB in-progress turn, its 73 collapsed sections expanded, and the turn's
final transcript streamed back in property-run chunks. It found five causes the
first replay could not reach, including per-render rereads of every archived
segment and an expanded hook audit that invalidated the retained live tail.
See ADR 0119's "keep a long live activity unit cheap to re-render" entry.

**How to apply:** reproduce a session's stall from a restored copy of that
session, at the state where it stalled, before trusting a replay's numbers.
Recipe that worked (harness was `.scratch/live-replay/`, local only):

- Copy the session directory to a temp root and keep `.lease/`: the
  publication head lives there. Drop `diagnostics/`.
- Before `mevedel-session-persistence-restore`, stub what the user's init
  provides: a gptel backend named in the sidecar (`gptel-make-openai "Codex"`),
  the preset (`gptel-make-preset 'mevedel-gpt6`), and `:reasoning-effort` on
  the model symbols. Stub `yes-or-no-p`/`y-or-n-p`.
- A stall inside an earlier request lives in an archived segment: load it with
  `mevedel-session-artifacts-read-segment` into the chat buffer, cut the tail,
  begin the live turn at the last user turn, expand sections, then feed the
  tail back and call `mevedel-view--flush-scheduled-render` per chunk.
- Compile with Eask first; `clean elc` leaves everything interpreted and
  distorts timings. Set `profiler-max-stack-depth` to 64; the default 16
  truncates render stacks and hides most of the cost.
- For before/after, byte-compile `git show HEAD:` copies of the touched modules
  into a separate directory and prepend it to `load-path`. Gate that on an
  environment variable carefully: `VAR=` sets an empty string, which
  `getenv` treats as set.
- Batch shows render cost only; a graphical child Emacs (`emacs -Q` with the
  Eask `load-path`) adds redisplay and queued-input latency.

Related: [Remote hook readiness and stdin timing](remote-hook-readiness-verification.md)
-- a green replay diagnoses nothing unless it imposes the real conditions.
