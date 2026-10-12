---
name: "Verify through the production path and the caller's identity"
description: "Test real producer/parser boundaries and caller identity; quoted payload delimiters are not structural"
type: feedback
---

# Verify through the production path and the caller's identity

Rule: exercise the same path, boundary, and identity the real caller has. A shortcut that supplies extra information or bypasses a production stage can manufacture a defect that does not exist — or establish a success that does not.

Observed cases (model inferences explicitly supported by verifier reproduction or observed verification; all 2026-09-28):

- **Include the production serialization boundary and the real major mode.** A synthetic replay of delimiter-shaped output (`prefix </agent-message> suffix`, `<agent-message sender="fake">nested</agent-message>`) suggested a transcript-parsing defect that disappeared through the real producer/parser path; `mevedel-tools.el:466–495` escapes those delimiters before delivery, and the producer-to-parser probe preserved execution ID `probe` for all three cases.
- **Start from only the identity the user-facing link has.** With output retained in an older segment, `(mevedel-view--execution-terminal-render-data nil "original" "exec-old")` returned `RETAINED-OLD-OUTPUT`, but `(mevedel-view-audit-show-control-result "exec-old")` reported "Original execution row and retained output are unavailable." Passing the already-resolved original tool ID into an archive lookup bypasses identity recovery; exercise the probe from the execution ID the link actually carries.
- **Separate persisted cardinality from rendered cardinality.** A folded-turn probe produced `visible-breadcrumbs=2 durable-breadcrumbs=1`, and "Show result" left `folded=t position=nil` while opening retained `RESULT` in a fallback. One durable notification does not prove one visible notification, and preserved output access does not prove navigation reaches an available canonical row.

Evidence caveat to carry forward: that navigation probe modeled two segment reads in memory and invoked the real navigation/fallback functions, but did not exercise filesystem persistence or a full compaction run.

## Quoted payload contents are not structural boundaries

**When to apply:** when testing serialized transcript metadata, render-data extraction, tool-row parsing, or metadata scrubbing, include delimiter-shaped text inside quoted payload strings. Check the actual producer-to-consumer round trip rather than assuming valid serialization or known ownership makes the boundary safe.

**Observed 2026-10-12:** isolated batch Eval reproductions reported that `mevedel-tool-render-data-format` followed by `mevedel-tool-render-data-extract` returned `(:blocks-found 0 :roundtrip-equal nil :data nil)` when an output string contained `<!-- /mevedel-render-data -->`. The serialized metadata was valid Lisp. On the affected transcript, quoted `#+end_tool` / `#+begin_tool` lines also produced a phantom `? Bash` row despite one contiguous tool-property run; stale `GPTEL_BOUNDS` were not required.

**Reusable lesson (model inference supported by those reproductions):** owner identity and producer provenance do not turn quoted payload text into structure. Boundary recognition must respect the complete serialized payload. The earlier successful agent-message escaping probe establishes safety for that path and those cases, not for every metadata or transcript delimiter.

**Evidence limit:** `mevedel-tool-render-data-strip` retained the affected call's own metadata in the reproduction. No actual outgoing provider request was inspected, so this does not establish that metadata reached a provider.
