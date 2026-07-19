# Bound inline agent results

Status: accepted

An automatic agent `RESULT` payload inlines at most 32 KiB, using the existing bounded head-and-tail preview and pointing to the child's persisted transcript for the complete response. Completed responses, error text, and interruption partials share the same limit. Mevedel reuses its current agent-transcript persistence rather than duplicating large results into a new storage system or every parent conversation.
