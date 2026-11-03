---
name: "Distinguish the composer from the pinned prompt header"
description: "An always-visible composer is not the pinned prompt header; preserve header readability and conversation continuity."
type: feedback
---

# Distinguish the composer from the pinned prompt header

## Establish which prompt surface the user means

User clarification, 2026-10-31 (`segment-0002.chat.org`, turn 299): the “always visible prompt” request concerned the **composer**, with child-frame and split-window composer prototypes as examples. The assistant acknowledged that it had incorrectly treated pinned-header work as satisfying that request.

When interpreting this request, distinguish:
- **Composer:** the editing surface where the user writes a prompt.
- **Pinned/sticky prompt header:** the displayed prompt retained in the view chrome.

Improving the header does not satisfy a request to keep the composer visible. The prototype references clarify the requested surface; they do not establish a selected implementation or prove either feature complete.

## Preserve pinned-header readability and continuity

Earlier user statements specifically concerned the sticky prompt:

- 2026-09-26 (`segment-0006.chat.org`, completed turn 13; screenshot `Bildschirmfoto_20260926_230041.png`): the sticky prompt worked, but sharing the status row made it “nearly always” truncate and cut off project information on the left. The user proposed a separate prompt line.
- 2026-09-27 (`segment-0008.chat.org`, completed turn 17): compaction is a continuation, so the sticky prompt should preserve the previous prompt; `/clear` starts a new conversation and should remove it. Transcript-segment boundaries are not conversation boundaries.

**Why:** conflating these surfaces can close the wrong feature request. Separately, sharing the status row degrades header readability, and clearing it at compaction discards expected continuity.

**How to apply:** establish the requested surface before planning or claiming completion. For sticky-header/status-zone changes, give the prompt its own line rather than sharing the status row, preserve it intact across compaction, and remove it on `/clear`. These header rules are not a broader composer-layout specification.
