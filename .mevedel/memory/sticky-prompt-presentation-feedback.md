---
name: "Sticky prompt presentation: keep it readable and treat compaction as continuation"
description: "Status-row sharing must not truncate the sticky prompt; compaction preserves the prompt, /clear removes it"
type: feedback
---

# Sticky prompt presentation: keep it readable and treat compaction as continuation

Two user statements about how the sticky prompt must behave in Mevedel's view chrome. Both are stated rules, not preferences inferred from silence.

- 2026-09-26 (user statement, `segment-0006.chat.org`, completed turn 13; screenshot `Bildschirmfoto_20260926_230041.png`): the sticky prompt itself worked, but sharing the status row made the prompt "nearly always" truncate and cut off project information on the left. The user proposed a separate prompt line. Design implication: keep prompt text readable without displacing project orientation.
- 2026-09-27 (user correction, `segment-0008.chat.org`, completed turn 17): compaction is a continuation, so the sticky prompt should preserve the previous prompt; `/clear` starts a new conversation and should remove it. Transcript-segment boundaries are not conversation boundaries.

**Why:** sharing a row with the status area silently degrades the prompt to a truncated stub, and treating a compaction boundary as a fresh start discards continuity the user expects.

**How to apply:** when touching sticky-prompt or status-zone rendering, give the prompt its own line rather than sharing the status row, keep the prompt intact and untruncated across compaction, and remove it on `/clear`. Do not generalize these into broader prompt-chrome redesigns beyond what the user stated.
