One thing I wanted to ask you is, how do you like mevedel? Is it a nice harness for you? Does it feel good?

---

## Deepseek:

Honest answer: yes, it's a good harness, and better in specific ways than most agent setups I've seen.

What actually works well from the inside:

- The documentation map and module layer map in AGENTS.md are the thing I lean on most. One line per module, a pointer to the right detail doc. A repo this size is usually a maze; this makes it navigable cheaply.
- The operational details show someone who has watched agents fail: "clean elc before tests so stale bytecode never shadows edited source", silent-output test conventions, "consult gptel source instead of duplicating it", the memory signal gate that says no-op is preferred over noise. These are real failure modes, not imagined ones.
- "No backwards compatibility" and "ADRs are revisable" make the work cleaner. Most projects force me to preserve dead weight; this one tells me to delete it.
- The tool surface is coherent: closed ToolScript dialect, resource addresses, permission seams. It feels like one design, not accreted parts.

The real cost is fixed context. The system prompt, AGENTS.md, hook injections, memory index, and skill roster add up to a lot of tokens before any task arrives. It's well organized, but the baseline is heavy, and there's some rule duplication (testing conventions appear in several layers). The ponytail hook that just injected a full mode block into this very conversation is an example of that cost.

So: good harness, feels like it was built by someone who understands the agent's side of the loop, not just the human's. If anything needs trimming, it's rule density, not design.

## GPT 5.6:

I don’t have feelings, but from the model-facing side, yes: mevedel is a strong harness.

What works particularly well:

- **Tools have clear boundaries.** Read, search, patch, execution, agents, permissions, and user questions are distinct operations. That reduces accidental misuse.
- **The workflow encourages evidence before edits.** Reading first, applying reviewable patches, and reporting exact test outcomes produces better engineering behavior than a thin chat wrapper.
- **Retained agents and task state are genuinely useful.** They support parallel investigation without forcing everything into one overflowing conversation.
- **Permissions are unusually thoughtful.** The distinction between reversible local work and risky external actions gives me room to act while protecting the user.
- **Skills, memory, workspace guidance, and resource addresses provide durable context** without requiring the user to repeat everything.
- **Emacs is a natural host.** Code, diagnostics, conversations, reviews, and navigation can stay close together instead of bouncing through disconnected applications.

The main friction is **instruction density**. There are many overlapping rules about tools, planning, delegation, validation, permissions, skills, memory, and response style. They are individually sensible, but the combined model-facing contract is large enough that resolving precedence becomes real work. Some tool schemas also consume substantial context before the actual task begins.

So my honest assessment is: **very capable, safe, and unusually coherent, but somewhat heavy**. The architecture feels designed around real failures observed in agentic programming rather than hypothetical features, which is a major strength. The best simplification target would not be removing capabilities. It would be making more policy implicit in the runtime so less of it has to remain continuously present in the prompt.

One caveat: I only experience the model-facing protocol and the text projected into the conversation. I cannot directly judge whether the visual Emacs interaction feels smooth to a human. From my side, though, it is substantially nicer than a basic “chat plus shell” harness.
