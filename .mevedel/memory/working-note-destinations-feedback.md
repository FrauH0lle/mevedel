---
name: "Put shareable working notes in work:// and memories, not .scratch"
description: "Authored notes/handoffs go to work://shared/ and memory; .scratch stays untracked and PC-local, plugin workflows untouched"
type: feedback
---

# Put shareable working notes in work:// and memories, not .scratch

User statement (captured 2026-09-21 final message, repeated in each 2026-09-22 session): the model tends to store and share information in ".sratch"; that location is local to this PC and ignored by Git. The intended destinations are `work://` and memories. Storage capability was never the problem — `Read("work://")` listed shared notes in every session that tried it.

**2026-09-24 user corrections (user statements, `segment-0001.chat.org`, completed turns 1–3).**

- `.scratch/` belongs to the https://github.com/mattpocock/skills plugin ecosystem; `work://` is mevedel machinery. Preserve the plugin workflows: "They are working really well and I dont want to break their workflow." The ordinary shared-notes default is not permission to rewrite plugin skills or relocate their workflow files.
- Nothing under `.scratch/` should be Git-tracked; `.mevedel/shared/` should be. The user rejected the proposed migration of the tracked `.scratch/directive-workflow-redesign/` files into shared work — they should simply be removed from tracking.
- For routing guidance the user wanted only `AGENTS.md` updated, leaving other documentation and the installed plugin skills alone. The model's broader "align repository docs and skill routing" proposal was declined; do not reintroduce it.

**Observed state (this review, 2026-09-27 pass).** The captured `AGENTS.md` "Working material" section now states the `work://shared/` default, keeps `.scratch/` untracked and local-only, preserves invoked skills' configured destinations, and describes `.mevedel/shared/` as versioned material. In the workspace `.gitignore` read during this review, `.scratch/` is still ignored (line 17) while `.mevedel/shared/` is explicitly exempted (line 38). Git ignore rules alone do not untrack files that are already tracked, so the tracking policy is user-stated intent plus a plausible ignore-file change, not proof that the cleanup was completed; check `git ls-files` under `.scratch/` before relying on it.

**Why this matters.** The recurring problem is where authored notes end up and what that destination guarantees, not missing capability.

- `work://shared/...` resolves under the workspace's `.mevedel/shared/`, survives session deletion, and is shared by sessions retaining that workspace, including mevedel-created worktree sessions; other `work://` descendants are session-owned. Shared notes are not injected into context automatically and require discovery, a read, or an explicit handoff address.
- Shared notes and memory are distinct destinations: curated memory is tracked and durable; `work://shared/` carries working material, and only Git commits and pulls move it between independent checkouts — the resource address itself does not.
- Working-note capture visits shared files before session-local files within a combined budget and does not scan `.scratch/` itself, so moving a note changes what can enter evidence, not just where the file lives. `.scratch/directive-workflow-redesign/` held 12 tracked PRD/issue files despite the ignore rule, and existing shared notes or handoffs still point into `.scratch` for full evidence; the blanket description "untracked scratch" was inaccurate.

**How to apply.**

- Default authored, shareable working notes and handoffs to `work://shared/`; promote durable lessons to memory. Do not use `.scratch/` as the storage for information meant to be shared.
- Keep genuinely local, disposable material in `.scratch/`: issue-tracker and skill workflow files, upstream dependency checkouts, raw test output, screenshots, generated artifacts. Do not propose a blanket ban or bulk migration, and do not rewrite plugin skills or other docs to enforce routing — the user chose an `AGENTS.md`-only change.
- Never track or commit anything under `.scratch/`; review `.mevedel/shared/` contents before committing, since that is the destination that is meant to be versioned.
- Changing a note's destination does not make its handoff self-contained; move or duplicate the linked evidence too.
- Status: the routing policy is now recorded in `AGENTS.md` and the `.mevedel/shared/` ignore exemption is present; whether the previously tracked `.scratch/` files were actually removed from tracking was not verified, and which material should travel between machines by what transport remains undecided.
