---
name: "Verify the real consumer boundary, not intermediate success"
description: "Use the production path and caller identity; distinguish transport, persistence, rendering, and behavioral acceptance."
type: feedback
---

# Verify the real consumer boundary, not intermediate success

**Rule:** test through the production path, using the identity and information the real caller has, and assert the outcome at the boundary the consumer actually uses. Internal flags, delivery receipts, persisted records, and constructed messages establish intermediate facts—not necessarily usable results, visible presentation, or accepted steering.

**Why:** shortcuts can manufacture both false failures and false confidence. These are model-inferred lessons from observed verification probes and the explicitly attributed user correction below, not a requirement to run every kind of end-to-end test for every change.

## Use the actual path and caller identity

Observed 2026-09-28:

- Delimiter-shaped agent output appeared to break a synthetic transcript parser, but the real producer escaped it and the producer-to-parser probe preserved the execution identity. Include production serialization and the real major mode before attributing a synthetic failure to the product.
- A retained-output lookup succeeded when supplied an already-resolved original tool ID, while the user-facing “Show result” entry point had only the execution ID and could not recover the output. Supplying information unavailable to the caller bypassed the failing boundary.
- A folded-turn probe had one durable breadcrumb but two visible breadcrumbs. A fallback could expose retained output while leaving the canonical row folded and unreachable. Persisted cardinality, visible cardinality, output access, and navigation are separate claims.

The navigation probe modeled two segment reads in memory and invoked real navigation/fallback functions; it did not establish filesystem persistence or a full compaction run.

## Assert consumed state, including boundary cases

Observed in verification probes on 2026-09-28:

- The verifier could retain full terminal output while guest current records were empty and guest archived history showed only initial output with a running header.
- A completion audit record existed and the original row was finished, but no breadcrumb appeared until a full rerender.
- An accepted `WriteStdin` operation had a successful internal control flag while downstream presentation showed failure after the process exited unsuccessfully.
- Retained-evidence fallback worked for nonempty output but treated present-and-empty output as unavailable.

**How to apply:**

- Assert rendered buffer text, guest reads, navigation, or retained-evidence access through the consumer's real entry point—not just an internal predicate or serialized record.
- Inspect immediate terminal-event presentation without forcing a redraw that could conceal a missing update.
- Cross segment, archive, projection, or renderer boundaries when the claim depends on them; same-segment coverage does not establish cross-segment behavior.
- Include absent, present-but-empty, and nonempty fixtures for evidence-presence and fallback logic.
- Distinguish a successful control action from the eventual outcome of the process it controls.

## Quoted payload contents are not structural boundaries

Observed 2026-10-12: isolated round-trip probes failed when an output string contained `<!-- /mevedel-render-data -->`, despite valid Lisp serialization. Quoted `#+end_tool` / `#+begin_tool` text also produced a phantom tool row without requiring stale `GPTEL_BOUNDS`.

**Lesson:** owner identity, valid serialization, and producer provenance do not make quoted payload text structural. Include delimiter-shaped strings in producer-to-consumer round trips and ensure boundary recognition respects the complete serialized payload.

The earlier successful agent-message escaping probe establishes only that path and those cases, not every transcript delimiter. The affected metadata-strip probe retained metadata, but no outgoing provider request was inspected; this does not establish provider exposure.

## Delivery evidence is not behavioral acceptance

Observed 2026-11-01 in “Background Job Load Test”: `claude-haiku-5-5` rejected two steering messages despite successful `PostToolBatch` hook receipts and a header asserting user authority. The user explicitly confirmed that the messages were theirs (“Yes, it was me”), correcting their classification as an injection attempt.

**Lesson:** when validating authorized steering, establish observable behavioral acceptance as well as delivery. A successful receipt or user-role transcript display does not show that the model recognized the message as user-authorized.

The model described the steering as “tool output,” while inspected native API-request blobs labeled the hook context `message.role: system`. The model's explanation is not proof of wire placement; final wire normalization was not established.

Adapter probes established message construction, not live model behavior. No live replay established reliable acceptance through the proposed ordinary-user-prompt route or a controlled Haiku-versus-Sonnet/Opus difference. The user reported acceptance by Opus/Sonnet; keep that report distinct from controlled observation.

This correction concerns genuine user-authored steering. It does not authorize treating arbitrary hook or tool content as user instructions.
