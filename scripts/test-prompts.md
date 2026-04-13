# Test Prompts for Multi-Agent / BWAIT Flow

Use these prompts to interactively test the coordinator and background
agent system.  After each test, run `/analyze-log test-log-1.log` to
verify the flow.

## Test 1: Basic Background + BWAIT

Exercises: background spawn, BWAIT parking, agent-result delivery,
coordinator synthesis.

```
Use your coordinator skill for this task: Spawn an explore agent to
count the number of `defun` forms in mevedel-tool-ui.el and another
explore agent to count the `defun` forms in mevedel-tools.el.  Wait
for both results and report a comparison table showing file, count,
and which file has more.
```

Expected trace:
- Coordinator spawns 2 explore agents (both `bg=true`)
- Coordinator stops → BWAIT parks (2 background agents pending)
- Each explore completes → agent-result pushed to coordinator mailbox
- After second agent completes → BWAIT resumes coordinator
- Coordinator synthesizes both results → reports to main

## Test 2: SendMessage Course-Correction

Exercises: background spawn, SendMessage delivery, MSG-INJECT.

```
Use your coordinator skill: Spawn an explore agent to survey
mevedel-presets.el.  After spawning, send it a message asking it to
specifically focus on how BWAIT transitions are injected.  Wait for
results and summarize.
```

Expected trace:
- Coordinator spawns explore (bg=true)
- Coordinator sends SendMessage → gets tool result
- Coordinator stops → BWAIT parks
- Explore turn 1 runs → gets first response
- Explore turn 2 receives MSG-INJECT with the guidance
- Explore completes → BWAIT resumes coordinator
- Coordinator synthesizes → reports to main

## Test 3: Quick Smoke Test (No Coordinator)

Exercises: foreground agent spawn only (no BWAIT needed).

```
Spawn an explore agent to count how many .el files are in the
project root.
```

Expected trace:
- Main spawns explore (foreground)
- Explore runs, returns result
- Main reports
- No BWAIT events

## Test 4: Stress - Three Background Agents

Exercises: multiple concurrent background agents, BWAIT with multiple
pending completions.

```
Use your coordinator skill: I need a quick comparison of three files.
Spawn three explore agents in parallel:
1. Count functions in mevedel-tool-fs.el
2. Count functions in mevedel-tool-code.el
3. Count functions in mevedel-tool-exec.el
Wait for all three and present results in a table with columns:
file, function count, largest function name.
```

Expected trace:
- Coordinator spawns 3 explore agents (all bg=true)
- BWAIT parks with 3 pending
- As each completes: background-agents count decreases
- After last completes → BWAIT resumes
- Coordinator synthesizes all 3 → reports
