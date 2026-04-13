#!/usr/bin/env python3
"""Analyze a gptel HTTP log file and produce a structured trace.

Parses the JSON request/response pairs logged by gptel and reconstructs
the conversation flow across agents, showing:

- Which agent each request belongs to (main, coordinator, explore, etc.)
- Message count and last user message summary
- Tool calls made per turn
- Agent-message / agent-result delivery
- Timestamps and token usage

Usage:
    python3 analyze-gptel-log.py LOG_FILE [--json] [--verbose]
"""

import json
import sys
import argparse
from typing import Optional


def identify_agent(system_content: str) -> str:
    """Identify the agent type from the system prompt content."""
    s = system_content.lower()[:500]
    if "coordinator" in s:
        return "coordinator"
    if "read-only exploration" in s or "exploration agent" in s:
        return "explore"
    if "planner" in s and "implementation plan" in s:
        return "planner"
    if "verifier" in s or "adversarial verification" in s:
        return "verifier"
    if "introspect" in s:
        return "introspector"
    if "tutor" in s and "socratic" in s:
        return "tutor"
    return "main"


def extract_tool_calls(msg: dict) -> list:
    """Extract tool call names from an assistant message."""
    calls = []
    for tc in msg.get("tool_calls", []):
        fn = tc.get("function", {})
        name = fn.get("name", "unknown")
        args_str = fn.get("arguments", "")
        try:
            args = json.loads(args_str) if isinstance(args_str, str) else args_str
        except json.JSONDecodeError:
            args = {}

        detail = ""
        if isinstance(args, dict):
            if name == "Agent":
                agent_type = (args.get("subagent_type")
                              or args.get("agent_type")
                              or args.get("type", "?"))
                bg = args.get("run_in_background", False)
                detail = f" type={agent_type}" + (" bg=true" if bg else "")
            elif name == "SendMessage":
                to = args.get("to", "?")
                detail = f" to={to}"
            elif name == "TaskCreate":
                title = args.get("title", "?")
                detail = f' "{title[:40]}"'
            elif name == "TaskUpdate":
                task_id = args.get("id", "?")
                status = args.get("status", "?")
                detail = f" id={task_id} status={status}"
            elif name == "Read":
                path = args.get("file_path", "?")
                detail = f" {path.split('/')[-1]}"
            elif name == "Glob":
                pattern = args.get("pattern", "?")
                detail = f" {pattern}"
            elif name == "Grep":
                pattern = args.get("pattern", "?")
                detail = f" /{pattern[:30]}/"
            elif name == "Skill":
                skill = args.get("name", "?")
                detail = f" {skill}"

        calls.append(f"{name}{detail}")
    return calls


def summarize_user_content(content) -> str:
    """Produce a short summary of user message content."""
    if content is None:
        return "(empty)"
    if isinstance(content, list):
        texts = []
        for part in content:
            if isinstance(part, dict):
                t = part.get("text", part.get("content", ""))
                if t:
                    texts.append(t[:100])
        return " | ".join(texts)[:200] if texts else "(structured)"
    text = str(content)
    if "<agent-result" in text:
        return "[AGENT-RESULT delivered]"
    if "<agent-message" in text:
        return "[AGENT-MESSAGE received]"
    if "<system-reminder>" in text:
        if "deferred tools" in text.lower() or "ToolSearch" in text:
            return "[system-reminder: deferred tools]"
        if "max-turns" in text.lower():
            return "[system-reminder: max-turns]"
        return "[system-reminder]"
    return text[:120]


def _parse_next_json(lines: list, start: int) -> Optional[dict]:
    """Parse the next JSON object starting from line index start."""
    i = start
    while i < len(lines) and lines[i].strip() != "{":
        i += 1
    if i >= len(lines):
        return None

    text = "\n".join(lines[i:])
    decoder = json.JSONDecoder()
    try:
        body, end = decoder.raw_decode(text)
        consumed_text = text[:end]
        lines_consumed = consumed_text.count("\n")
        body["_end_line"] = i + lines_consumed + 1
        return body
    except json.JSONDecodeError:
        return None


def parse_log(filepath: str) -> list:
    """Parse a gptel log file into a list of chronological events.

    Returns events in log order.  Each event has a 'type' key:
      'request'  - an outgoing API request
      'response' - an incoming API response (non-streaming)
      'stream_finish' - a streaming response completion
    """
    with open(filepath) as f:
        lines = f.readlines()

    events = []
    decoder = json.JSONDecoder()

    # Find all marker lines: { "gptel": "...", "timestamp": "..." }
    # Each marker is a small JSON object spanning 3-4 lines.
    markers = []
    i = 0
    while i < len(lines):
        if lines[i].strip() == "{" and i + 1 < len(lines) and '"gptel"' in lines[i + 1]:
            # Found a marker start — collect until closing }
            j = i + 1
            while j < len(lines) and lines[j].strip() != "}":
                j += 1
            marker_text = "".join(lines[i : j + 1])
            try:
                marker = json.loads(marker_text)
                markers.append((i, j + 1, marker))
            except json.JSONDecodeError:
                pass
            i = j + 1
        else:
            i += 1

    # Process markers in order
    for marker_idx, (start_line, after_marker_line, marker) in enumerate(markers):
        gptel_type = marker.get("gptel", "")
        timestamp = marker.get("timestamp", "?")

        if gptel_type == "response headers":
            continue

        body = _parse_next_json(lines, after_marker_line)
        if not body:
            continue

        if gptel_type == "request body":
            msgs = body.get("messages", [])
            sys_content = msgs[0].get("content", "") if msgs else ""
            agent = identify_agent(sys_content)

            # Last user message
            last_user = None
            for m in reversed(msgs):
                if m.get("role") == "user":
                    last_user = m
                    break

            # Check last user message specifically for agent XML tags
            has_agent_result = False
            has_agent_msg = False
            if last_user:
                luc = str(last_user.get("content", ""))
                if "<agent-result" in luc:
                    has_agent_result = True
                if "<agent-message" in luc:
                    has_agent_msg = True

            events.append({
                "type": "request",
                "line": start_line + 1,
                "timestamp": timestamp,
                "agent": agent,
                "num_messages": len(msgs),
                "last_user": summarize_user_content(
                    last_user.get("content") if last_user else None
                ),
                "has_agent_result": has_agent_result,
                "has_agent_message": has_agent_msg,
            })

        elif gptel_type == "response body":
            # Check for error responses (429, 500, etc.)
            if "error" in body and "choices" not in body:
                err = body["error"]
                err_msg = (err.get("message", str(err))
                           if isinstance(err, dict) else str(err))
                err_code = (err.get("code", "?")
                            if isinstance(err, dict) else "?")
                events.append({
                    "type": "error",
                    "line": start_line + 1,
                    "timestamp": timestamp,
                    "error_code": str(err_code),
                    "error_message": err_msg[:200],
                })
                continue

            choices = body.get("choices", [])
            if not choices:
                continue
            choice = choices[0]
            msg = choice.get("message", choice.get("delta", {}))
            content_text = msg.get("content", "") or ""
            finish = choice.get("finish_reason", "")
            tool_calls = extract_tool_calls(msg)
            usage = body.get("usage", {})

            if finish:
                events.append({
                    "type": "response",
                    "line": start_line + 1,
                    "timestamp": timestamp,
                    "finish_reason": finish,
                    "content_preview": content_text[:200],
                    "tool_calls": tool_calls,
                    "prompt_tokens": usage.get("prompt_tokens", 0),
                    "completion_tokens": usage.get("completion_tokens", 0),
                })

    # Also pick up streaming finishes (SSE data lines)
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("data: {") and '"finish_reason"' in stripped:
            try:
                data = json.loads(stripped[6:])
                choices = data.get("choices", [])
                if choices:
                    fr = choices[0].get("finish_reason")
                    if fr:
                        usage = data.get("usage", {})
                        events.append({
                            "type": "stream_finish",
                            "finish_reason": fr,
                            "prompt_tokens": usage.get("prompt_tokens", 0),
                            "completion_tokens": usage.get("completion_tokens", 0),
                        })
            except json.JSONDecodeError:
                pass

    return events


def format_trace(events: list, verbose: bool = False) -> str:
    """Format events into a human-readable trace."""
    output = []
    output.append("=" * 70)
    output.append("GPTEL LOG TRACE")
    output.append("=" * 70)

    # Track per-agent turn counts and assign responses to preceding requests
    agent_turns = {}
    last_request_agent = None
    turn = 0

    for evt in events:
        if evt["type"] == "request":
            turn += 1
            agent = evt["agent"]
            agent_turns[agent] = agent_turns.get(agent, 0) + 1
            last_request_agent = agent

            flags = []
            if evt.get("has_agent_result"):
                flags.append("BWAIT-RESUME")
            elif evt.get("has_agent_message"):
                flags.append("MSG-INJECT")

            flag_str = f"  [{', '.join(flags)}]" if flags else ""

            output.append("")
            output.append(
                f"[{evt['timestamp']}] REQ  {agent.upper()} "
                f"(turn {agent_turns[agent]}, {evt['num_messages']} msgs)"
                f"{flag_str}"
            )
            output.append(f"  Last user: {evt['last_user']}")

        elif evt["type"] == "response":
            finish = evt["finish_reason"]
            tools = evt.get("tool_calls", [])
            tok = f"{evt['prompt_tokens']}p+{evt['completion_tokens']}c"

            if finish == "tool_calls" and tools:
                output.append(
                    f"[{evt['timestamp']}] RESP tool_calls ({tok}): "
                    f"{', '.join(tools)}"
                )
            elif finish == "stop":
                preview = evt["content_preview"].replace("\n", " ")[:100]
                output.append(
                    f"[{evt['timestamp']}] RESP stop ({tok}): "
                    f'"{preview}..."'
                )
            else:
                output.append(
                    f"[{evt['timestamp']}] RESP {finish} ({tok})"
                )

        elif evt["type"] == "error":
            output.append(
                f"[{evt['timestamp']}] ERROR {evt['error_code']}: "
                f"{evt['error_message']}"
            )

        elif evt["type"] == "stream_finish":
            tok = ""
            if evt.get("prompt_tokens"):
                tok = f" ({evt['prompt_tokens']}p+{evt['completion_tokens']}c)"
            output.append(f"  [stream] finish={evt['finish_reason']}{tok}")

    # Summary
    output.append("")
    output.append("=" * 70)
    output.append("SUMMARY")
    output.append("=" * 70)
    output.append(f"Total API requests: {sum(1 for e in events if e['type'] == 'request')}")
    output.append(f"Total API responses: {sum(1 for e in events if e['type'] in ('response', 'stream_finish'))}")
    error_count = sum(1 for e in events if e["type"] == "error")
    if error_count:
        output.append(f"Total API errors: {error_count} *** CHECK TRACE ***")
    for agent, count in sorted(agent_turns.items()):
        output.append(f"  {agent}: {count} requests")

    # BWAIT indicators
    bwait_events = [
        e for e in events
        if e["type"] == "request" and e.get("has_agent_result")
    ]
    msg_events = [
        e for e in events
        if e["type"] == "request" and e.get("has_agent_message") and not e.get("has_agent_result")
    ]

    if bwait_events:
        output.append("")
        output.append("Agent-result delivery (BWAIT -> WAIT resume):")
        for e in bwait_events:
            output.append(f"  {e['agent']} at {e['timestamp']}")

    if msg_events:
        output.append("")
        output.append("Agent-message injection (SendMessage delivery):")
        for e in msg_events:
            output.append(f"  {e['agent']} at {e['timestamp']}")

    return "\n".join(output)


def main():
    parser = argparse.ArgumentParser(description="Analyze gptel HTTP log files")
    parser.add_argument("logfile", help="Path to the gptel log file")
    parser.add_argument("--json", action="store_true", help="Output as JSON")
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")
    args = parser.parse_args()

    events = parse_log(args.logfile)

    if args.json:
        # Filter out internal keys
        clean = []
        for e in events:
            clean.append({k: v for k, v in e.items() if not k.startswith("_")})
        print(json.dumps(clean, indent=2, default=str))
    else:
        print(format_trace(events, verbose=args.verbose))


if __name__ == "__main__":
    main()
