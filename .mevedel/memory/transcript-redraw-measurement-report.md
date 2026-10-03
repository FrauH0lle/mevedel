---
name: "Terminal transcript-redraw latency measurement report"
description: "Checked redraw/latency measurements for commit 51a2ffe live in work://shared/transcript-redraw-2026-09-27/"
type: reference
---

# Terminal transcript-redraw latency measurement report

Authoritative source for recent-session transcript-redraw measurements: `work://shared/transcript-redraw-2026-09-27/report.md` (independently checked), covering commit `51a2ffe`. Adjacent `results.json` and harness files there preserve fixture identities, raw-result pointers, and reproducible comparisons of cold synchronous, warm scheduled, and cold-cache scheduled projection.

**Purpose:** cite it when judging terminal keyboard-to-command latency changes instead of re-measuring or extrapolating from memory. Read the report for numbers; this note only points to it.

**Scope limits:** terminal keyboard-to-command latency only — not graphical keystroke-to-screen latency, and not full-session resume. Reported 2026-09-27 (observed outcome, `segment-0001.chat.org`, completed turn 4). Shared notes are not injected into context automatically, so the path must be read explicitly.
