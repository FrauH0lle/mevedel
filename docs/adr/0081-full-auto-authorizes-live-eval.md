# Full-auto authorizes live Eval

Status: accepted

Selecting `full-auto` authorizes model-generated live Eval without a separate
permission rule or prompt, even though live Eval executes inside Emacs and
cannot use child-process confinement. This follows the mode's contract of
granting Full Access under the OS account; the UI and model reminder must disclose
that live Eval is inherently unconfined.

The full-auto reminder states: "Live Eval runs inside Emacs without confinement."
The tool description and permission card also disclose this execution boundary.

## Decision history

ADR 0013 made the same choice: full-auto is deliberate authority for unattended
live Eval despite the absence of child-process or allowed-root confinement;
explicit denies still apply. ADR 0081 restated the decision and its disclosure
requirement rather than reversing it. Users who require an ordinary live-Eval
prompt select `edits` or `ask`.
