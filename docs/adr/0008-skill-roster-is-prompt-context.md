# Keep skill discovery separate from invocation

Status: accepted. Catalog transport follows [ADR 0115](0115-retain-delivered-conversation-fragments.md).

## Current decision

Skills are discoverable capabilities, with explicit invocation and preparation
boundaries. A stable system component explains user invocation, optional
selection, and guidance lifetime. A compact catalog arrives through retained
current-context updates. It contains enabled, model-invocable skills without
path restrictions, using canonical names, registered `skill://` source addresses,
and short purposes. Detailed descriptions remain searchable through ListSkills.
Read inspects source; Skill prepares and invokes it. Neither discovery nor source
inspection proves that a skill has been invoked.

The catalog budget defaults to 2% of the effective request model's context
window, estimated at four characters per token. Purposes use the first sentence
or line, capped at 160 characters. Descriptions shrink before entries disappear.
The stable policy advertises ListSkills even when the catalog is empty or cannot
fit. Display names are UI-only; canonical names include source/plugin prefixes
when required to disambiguate invocation.

Path-scoped skills use optional recipient-local notices and ListSkills. Matching
file activity affects catalog eligibility, not required workflow or authority.
A query can find dormant skills; exact invocation may use one without activating
its default-listing visibility. Notice acknowledgement occurs only on delivery,
independently for each conversation. A changed skill fact can rearm a notice;
compaction alone does not. Cold resume starts a new live notice throttle.

Explicit user requests and prepared dependencies are honored. Other guidance is
selected for its useful scope and approach. A new message does not automatically
end its applicability; authored scope, task completion, and user direction do.
Enablement is separate from invocation arguments such as `$foo off`.

Leading user commands and model-side Skill calls own invocation policy. Inline
user mentions attach instructions. Required `!$skill` dependencies also attach
instructions; they never inherit command, agent, model, effort, hook, or consuming
request permission policy. Only literal authored declarations create dependency
structure. Origin and source identity survive preparation, so generated text
cannot create dependencies or bypass a model-invocation gate. Graph failure
prevents partial request/child dispatch, but completed body injections cannot be
rolled back.

The [skills manual](../skills.md) owns discovery roots, syntax, soft unavailable
mentions, hook ordering, request ownership, frontmatter, and preparation details.
The built-in worker and explorer have skill capabilities; verifier and reviewer
do not. Skill remains read-only in the tool classification because actual
injection or child effects pass through their own permission-gated tools.

## Rationale and consequences

Discovery should be cheap without making a description match an unconditional
workflow. Explicit preparation centralizes source resolution, dependency checks,
and invocation policy instead of requiring models to recreate them by reading
files. Exact source addresses enable inspection without host-path rediscovery.

Keeping changing catalogs out of the system prefix preserves earlier request
content and updates retained workers. Recipient-local acknowledgements prevent a
child's observation from suppressing its parent's notice or a cancelled request
from consuming its own retry. Optional notices can leave context; ListSkills is
the rediscovery path, while required body fidelity belongs to compaction.

Required attachments allow shared authored rules without copying them into every
leaf skill. Literal provenance and inherited origin constrain this exception to
recursive interpretation. Structured render metadata keeps the user-facing
prepared body and dependency names independent of provider reminder wrappers.

## Decision history

- **ADR 0008 initially placed the roster at the tail of the system prompt**, with
  no source paths and no section for an empty catalog. This replaced a recurring
  full-roster reminder. A 2026-09-07 lifecycle measurement found that path activity
  added a whole Skills section in Luna and Sol follow-ups and fresh restore
  removed it, invalidating the prefix before all history. Path-scoped entries
  were excluded from that roster. ADR 0115 subsequently moved the changing
  catalog to retained conversation updates because ordinary configuration
  changes still invalidated the system prefix and frozen workers became stale.
  The former persisted root `skills-snapshot` and capped ten-item change-delta
  reminder were removed in favor of actual-payload observation acknowledgement.
- **Shared path-notice acknowledgement was replaced with per-conversation
  delivery facts.** A child could suppress its parent's discovery, and cancelled
  staging could suppress a later retry. Per-skill event keys also preserve
  distinct discoveries in multi-path calls. The optional notice does not claim
  that a frozen system roster changed or enforce pre-action instruction loading.
- **Mandatory match-and-invoke, ordering announcements, and one-turn lifetimes
  were removed.** Captured prompts repeated those rules while disagreeing about
  task versus turn scope. Stable optional-selection policy replaced them without
  rewriting skill authors' own requirements.
- **Literal required attachments replaced copying shared rules.** Their source
  provenance prevents argument substitution, hooks, injection results, or model
  output from creating dependency structure. A flattened presentation later
  misparsed an authored `<system-reminder>` example as the generated wrapper's
  end. Structured root/dependency metadata and balanced transcript scanning
  replaced wrapper-based presentation recovery.
- **Source inspection gained registered addresses** under
  [ADR 0104](0104-keep-resource-addresses-closed-and-capability-neutral.md), superseding the original
  no-path catalog choice while keeping raw backing paths out of the roster.
  User invocation binding and unavailable-mention behavior follow
  [the mention lifecycle](../mentions.md#atomic-binding-lifecycle); the original
  blanket rejection of known disabled inline mentions is no longer current.
- **Tutor removal** is recorded once in
  [ADR 0070](0070-compose-system-prompts-from-ordered-profiles.md#decision-history).
  It did not change skill discovery or invocation boundaries.

- **Frontmatter parsing gained a file-fingerprint cache.** A startup profile with
  38 skills put YAML parsing at 0.8 seconds and one third of command allocation.
  Reusing unchanged file parses reduced repeated scanning work while retaining
  hot reload; directly supplied content remains uncached because a file
  fingerprint cannot prove its identity.
