# Isolate guardian prompt boundaries

Both model-backed guardians use a dedicated trusted system message and receive
the material under review only as untrusted user content. They do not inherit
the session's coding-assistant prompt or ambient transcript, tools, memories,
skills, and workspace instructions. This keeps reviewer policy above
potentially adversarial evidence without paying the cost and distraction of the
full session prompt. Each guardian owns a separate, complete system prompt;
their small shared trust-boundary wording is repeated rather than abstracted
because their authority, review criteria, and response contracts differ. The
Goal guardian remains tool-free and trusts explicit references to existing PRDs
or tickets instead of expanding or independently reviewing them, keeping plan
approval distinct from repository investigation. Each complete system prompt
lives in its own prompt file, while Elisp constructs the untrusted user
evidence; no shared prompt template is introduced.
