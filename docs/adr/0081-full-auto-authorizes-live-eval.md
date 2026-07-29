# Full-auto authorizes live Eval

Status: accepted

Selecting `full-auto` authorizes model-generated live Eval without a separate
permission rule or prompt, even though live Eval executes inside Emacs and
cannot use child-process confinement. This follows the mode's contract of
removing heuristic execution prompts; the UI and model reminder must disclose
that live Eval is inherently unconfined.
