---
name: analyze-log
description: Analyze a gptel HTTP log file and trace the multi-agent conversation flow
context: inline
user-invocable: true
model-invocable: false
argument-hint: <log-file-path>
---

Analyze the gptel HTTP log file at the path given in `$ARGUMENTS`.

If no path is given, look for the most recent `*.log` file in the
current workspace root.

Run the analyzer script:

```!
python3 ${CLAUDE_SKILL_DIR}/../../scripts/analyze-gptel-log.py $ARGUMENTS
```

After seeing the trace output, provide a brief interpretation covering:

1. **Flow correctness**: Did requests flow in the expected order?
   Did each agent get the right number of turns?
2. **BWAIT events**: Were there any BWAIT-RESUME markers? If so,
   which agent received agent-results and when?
3. **Message injection**: Were any SendMessage deliveries visible
   (MSG-INJECT markers)?
4. **Anomalies**: Any missing responses, unexpected agent counts,
   or mismatched token counts?
5. **Timing**: How long did the full flow take? Any suspiciously
   long gaps?

Keep the interpretation concise (under 200 words).
