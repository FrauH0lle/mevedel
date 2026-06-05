# Architecture Decision Records

Record durable architectural decisions here when they should guide future work.

```mermaid
flowchart TD
    A[Decision with meaningful alternatives] --> B[Discuss tradeoffs]
    B --> C{Future work should be guided by it?}
    C -- No --> D[Do not create ADR]
    C -- Yes --> E[Create numbered ADR]
    E --> F[Reference ADR during related work]
    F --> G{Decision no longer fits?}
    G -- Yes --> H[Write superseding ADR]
    G -- No --> F
```

Add ADRs lazily: create one when a decision has meaningful alternatives, tradeoffs, and future consequences. Use short numbered filenames such as `0001-session-persistence-layout.md`.
