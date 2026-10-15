---
name: "Protect long-running work without treating silence as refusal"
description: "Sleep blocking protects work and remote reachability; permission absence timeouts are opt-in, not implicit refusals"
type: user
---

# Protect long-running work and remote reachability

User statements and corrections, 2026-10-15:

- Sleep inhibition should block sleep, not monitor blanking or locking. Its reason text should identify active reasons.
- A concrete collaboration use case is starting a whiteboard share on the desktop, continuing from a laptop, and leaving for about 30 minutes. The desktop must remain reachable; current guest presence alone does not capture that need.
- Commands should block sleep to protect running work. Legitimate commands can take 20 minutes, including mevedel's tests; duration alone does not justify termination.
- Users expect unanswered permission requests to block indefinitely. Any absence timeout should be opt-in through a `defcustom`, off by default. If enabled, the user preferred 30–60 minutes over five minutes. The motivation was interaction with sleep blocking, not treating silence as refusal.
- The harness, rather than the model, should enforce Bash timeouts.

Before proposing automatic termination or extra safeguards against indefinite sleep inhibition, distinguish a possible risk from measured frequency. This conversation supplied no measurement of how often abandoned commands hold inhibition indefinitely.

These preferences do not establish that a default execution timeout currently exists.
