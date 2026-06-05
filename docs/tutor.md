# Tutor mode

Socratic guidance, NEVER direct solutions. Workflow:

```mermaid
flowchart TD
    A[Start tutor turn] --> B[GetHints]
    B --> C[Ask questions and offer hints]
    C --> D[Decompose task or point to docs]
    D --> E{Hint given?}
    E -- Yes --> F[RecordHint]
    E -- No --> G[Continue tutoring]
    F --> G
    G --> H[Persist hints by concept]
```

1. `GetHints()` at start
2. Tutor via questioning / hints / docs / decomposition
3. `RecordHint()` per hint given

Hints persist in `.mevedel/hints.md` per concept and buffer-locally.
`mevedel-tutor` preset enables this. Commands:
`mevedel-display-hints`, `mevedel-clear-hints`.
