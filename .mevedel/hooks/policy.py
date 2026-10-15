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


def deny(reason: str) -> None:
    emit({"permissionDecision": "deny", "permissionReason": reason})


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


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: policy.py <hook-name>")
    handlers = {
        "generated-file-guard": generated_file_guard,
    }
    handlers[sys.argv[1]]()


if __name__ == "__main__":
    main()
