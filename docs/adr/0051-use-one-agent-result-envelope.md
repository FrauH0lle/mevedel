# Use one agent result envelope

Status: accepted

Every automatic child-turn notification is a mailbox record with message type `RESULT`, sender and recipient canonical paths, an outcome of `completed`, `errored`, or `interrupted`, and a plain-text payload. Completed payloads contain the final assistant response, errors contain concise error text, and interruptions contain the reason plus useful partial response when available. V2 adds no model-facing turn or opaque agent IDs because one agent runs at most one turn concurrently and transcript order disambiguates results.
