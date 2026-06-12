#!/usr/bin/env python3
"""Project-local mevedel hook policy helpers."""

from __future__ import annotations

import json
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


def path_value(data: dict[str, object]) -> str:
    value = tool_input(data).get("file_path")
    return value if isinstance(value, str) else ""


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
        (r"(^|[;&|()[:space:]])rm[[:space:]].*(-[A-Za-z]*r[A-Za-z]*f|-rf|-fr)", "rm -rf is blocked"),
        (r"git[[:space:]]+reset[[:space:]]+--hard\b", "git reset --hard is blocked"),
        (r"git[[:space:]]+clean[[:space:]].*(-[A-Za-z]*f[A-Za-z]*d|-[A-Za-z]*d[A-Za-z]*f)", "git clean -fd is blocked"),
        (r"git[[:space:]]+push\b.*(--force|-f|--force-with-lease)", "force-push is blocked"),
        (r"\.git(/|$)", "commands touching .git internals are blocked"),
    ]
    for pattern, reason in dangerous:
        if re.search(pattern.replace("[[:space:]]", r"\s"), cmd):
            deny(reason)
            return


def generated_file_guard() -> None:
    path = path_value(payload())
    blocked = [
        (path.endswith(".elc"), "generated .elc files must not be edited"),
        ("/.mevedel/sessions/" in path or path.startswith(".mevedel/sessions/"), "session transcripts are generated runtime artifacts"),
    ]
    for matched, reason in blocked:
        if matched:
            deny(reason)
            return


def post_edit_context() -> None:
    path = path_value(payload())
    if path.endswith(".el"):
        additional_context(
            "An Emacs Lisp file was edited. Before final response, run the focused ERT test if one matches the module, and run `npx @emacs-eask/cli compile` for shared or cross-module changes. Keep byte compiler output warning-free."
        )


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


def stop_validation_context() -> None:
    additional_context(
        "If source files were changed this turn and validation was not run, mention the missing focused test/compile/check explicitly instead of implying the change is fully green."
    )


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: policy.py <hook-name>")
    handlers = {
        "bash-safety": bash_safety,
        "generated-file-guard": generated_file_guard,
        "post-edit-context": post_edit_context,
        "precompact-context": precompact_context,
        "subagent-context": subagent_context,
        "risky-skill-context": risky_skill_context,
        "stop-validation-context": stop_validation_context,
    }
    handlers[sys.argv[1]]()


if __name__ == "__main__":
    main()
