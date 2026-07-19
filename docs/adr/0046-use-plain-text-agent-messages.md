# Use plain-text agent messages

Status: accepted

The `message` fields of `Agent`, `SendMessage`, and `FollowupAgent` accept one non-empty plain-text string. V2 does not add structured message items, attachments, metadata envelopes, or dual text/item formats. Agents share the workspace and may reference files by path; richer payloads should be introduced only for a demonstrated use case.
