---
name: "Working notes are temporary; extract durable knowledge automatically"
description: "Use temporary machine-local shared notes, preserve skill workflows, and make selected automatic extraction genuinely automatic."
type: feedback
---

# Working notes are temporary; extract durable knowledge automatically

## User intent and corrections

- **2026-09-21, repeated 2026-09-22:** The user corrected the tendency to store information intended for sharing in `.scratch/`. The intended destinations for ordinary working notes and durable lessons were `work://` and memory, respectively; storage capability was not the problem.
- **2026-09-24, `segment-0001.chat.org`, completed turns 1–3:** Preserve the Matt Pocock plugin skills' configured workflows and destinations. The user said they were working well and did not want them broken. The ordinary working-notes default is not permission to rewrite plugin skills or relocate their workflow files. Nothing under `.scratch/` should be tracked or committed; the proposed migration of tracked `.scratch/directive-workflow-redesign/` files into shared work was rejected in favor of removing them from tracking. For that routing change, the user requested only `AGENTS.md`, not a broader documentation-and-skills rewrite.
- **2026-10-10/11, session `2026-10-11T00-23-28ddf8da109b`, `segment-0001.chat.org`, completed turns 1–2:** Shared content is a project-level exchange, not necessarily permanent; memory holds permanent knowledge. The user then clarified that `work://shared/` should be temporary, machine-local material, not Git-mediated sharing across machines. They had originally tracked it to share between machines but no longer considered that useful. This supersedes the September preference to version `.mevedel/shared/`.
- **2026-10-11, completed turn 3:** Automatic extraction is necessary because manual extraction is easily forgotten. When the user selects automatic operation, “automatic means automatic”—do not prompt them to initiate or confirm that selected operation.
- **2026-10-11, completed turns 5–6:** Keep the shared-notes folder clean, including removing empty folders. Lean toward cleanup/removal when usefulness is uncertain. The user explicitly reinstated shortening or rewriting a note; this is not a blanket ban on condensation.

## How to apply

Use `work://shared/` for ordinary temporary notes and handoffs while preserving explicitly invoked skills' workflows. Extract qualifying durable knowledge reliably before cleanup rather than treating shared notes as permanent storage.

For selected automatic extraction, do not substitute a manual reminder or recurring confirmation prompt. Delete uncertain-use material; condense a note when a concrete continuing-use portion remains. Preserve useful extracted knowledge before removing its source.

Keep `.scratch/` local and untracked. Do not turn the shared-notes default into permission to break plugin workflows or perform the broader routing rewrite the user rejected. Consult current project instructions for maintained storage mechanics rather than relying on historical Git-state observations.
