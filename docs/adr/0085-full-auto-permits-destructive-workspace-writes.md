# Full-auto permits destructive workspace writes

Status: accepted

With no explicit deny and no guardian veto, `full-auto` executes dangerous
commands such as `rm -rf .` when their effects stay within already-authorized
workspace resources. Adding a hidden catastrophic-command prompt would
contradict the mode's no-heuristic-prompts contract. Users who want a
deterministic stop use a deny rule, enable guardian review, or select a less
autonomous permission mode.
