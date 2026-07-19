# Agent follow-ups retain conversation context

Status: accepted

An agent identity owns a continuing conversation across its turns. A follow-up task resumes that conversation with its prior context intact; spawning another agent creates a fresh conversation. Callers should therefore reuse an agent for related work and spawn a new one when old context would be irrelevant or contaminating.
