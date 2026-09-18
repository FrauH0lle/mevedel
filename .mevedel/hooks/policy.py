#!/usr/bin/env python3
"""Project-local mevedel hook policy helpers."""

from __future__ import annotations

import json
import posixpath
import re
import sys


def emit(decision: dict[str, object]) -> None:
    print(json.dumps(decision, separators=(",", ":")))


def payload() -> dict[str, object]:
    try:
        return json.load(sys.stdin)
    except json.JSONDecodeError as exc:
        emit(
            {
                "continue": False,
                "stopReason": f"invalid hook payload: {exc}",
            }
        )
        raise SystemExit(0)


def tool_input(data: dict[str, object]) -> dict[str, object]:
    value = data.get("tool_input")
    return value if isinstance(value, dict) else {}


def command(data: dict[str, object]) -> str:
    value = tool_input(data).get("command")
    return value if isinstance(value, str) else ""


def patch_paths(data: dict[str, object]) -> list[str]:
    """Read normalized file and rename paths from native ApplyPatch headers."""
    patch = tool_input(data).get("patch")
    if not isinstance(patch, str):
        return []
    paths = []
    for line in patch.replace("\r", "\n").split("\n"):
        match = re.fullmatch(
            r"\*\*\* (?:(?:Add|Update|Delete) File|Move to): (.+)",
            line.strip(" \t\r\n"),
        )
        if match:
            paths.append(posixpath.normpath(match[1].strip(" \t\r\n")))
    return paths


def skill_name(data: dict[str, object]) -> str:
    value = data.get("skill_name")
    return value if isinstance(value, str) else ""


def deny(reason: str) -> None:
    emit({"permissionDecision": "deny", "permissionReason": reason})


def additional_context(text: str) -> None:
    emit({"additionalContext": text})


def bash_safety() -> None:
    cmd = command(payload())
    dangerous = [
        (r"(^|[;&|()\s])rm\s.*(-[A-Za-z]*r[A-Za-z]*f|-rf|-fr)", "rm -rf is blocked"),
        (r"git\s+reset\s+--hard\b", "git reset --hard is blocked"),
        (r"git\s+clean\s.*(-[A-Za-z]*f[A-Za-z]*d|-[A-Za-z]*d[A-Za-z]*f)", "git clean -fd is blocked"),
        (r"git\s+push\b.*(--force|-f|--force-with-lease)", "force-push is blocked"),
        (r"\.git(/|$)", "commands touching .git internals are blocked"),
    ]
    for pattern, reason in dangerous:
        if re.search(pattern, cmd):
            deny(reason)
            return


def generated_file_guard() -> None:
    for path in patch_paths(payload()):
        blocked = [
            (path.endswith(".elc"), "generated .elc files must not be edited"),
            ("/.mevedel/sessions/" in path or path.startswith(".mevedel/sessions/"), "session transcripts are generated runtime artifacts"),
        ]
        for matched, reason in blocked:
            if matched:
                deny(reason)
                return


def precompact_context() -> None:
    additional_context(
        "Preserve the current objective, changed file paths/functions, failing diagnostics or tests, outstanding tasks, reviewer/verifier verdicts, hook or permission decisions, agent transcript handles, and explicit user constraints."
    )


def subagent_context() -> None:
    additional_context(
        "This is the mevedel Emacs Lisp repository. For Emacs Lisp investigation, prefer loaded-session introspection tools when available. Read relevant docs/*.md before architecture claims. Never edit generated artifacts such as *.elc. Use AGENTS.md testing commands. Reviewer/verifier findings should include exact file:line references."
    )


def risky_skill_context() -> None:
    if skill_name(payload()) in {"triage", "to-issues", "to-prd", "setup-skills"}:
        additional_context(
            "Before external/shared mutations such as publishing issues, PRDs, labels, or tracker changes, summarize exact intended actions and ask the user for confirmation. Do not create docs, issues, or tracker updates until approved."
        )


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: policy.py <hook-name>")
    handlers = {
        "bash-safety": bash_safety,
        "generated-file-guard": generated_file_guard,
        "precompact-context": precompact_context,
        "subagent-context": subagent_context,
        "risky-skill-context": risky_skill_context,
    }
    handlers[sys.argv[1]]()


if __name__ == "__main__":
    main()
