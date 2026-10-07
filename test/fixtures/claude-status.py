#!/usr/bin/env python3
"""Supported Claude CLI status surface, without credentials or model calls."""

import json
import os
import pathlib
import shutil
import sys
import time

if os.getenv("MEVEDEL_TEST_STATUS_LOG"):
    with open(os.environ["MEVEDEL_TEST_STATUS_LOG"], "a", encoding="utf-8") as log:
        log.write(os.path.basename(sys.argv[0]) + " " + " ".join(sys.argv[1:]) + "\n")
time.sleep(float(os.getenv("MEVEDEL_TEST_STATUS_DELAY", "0")))

if os.path.basename(sys.argv[0]) == "npm":
    if sys.argv[1] == "view":
        print('"0.86.0"')
        sys.exit(0)
    target = pathlib.Path(sys.argv[sys.argv.index("--prefix") + 1])
    target.mkdir(parents=True, exist_ok=True)
    (target / "install-invocation.json").write_text(json.dumps(sys.argv[1:]))
    time.sleep(0.1)
    if os.getenv("MEVEDEL_TEST_INSTALL_FAIL"):
        print("Fixture installation failed")
        sys.exit(1)
    adapter = target / "node_modules" / ".bin" / "claude-agent-acp"
    adapter.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(__file__, adapter)
    adapter.chmod(0o700)
    print("Fixture adapter installed")
elif sys.argv[1:] == ["--version"]:
    if os.path.basename(sys.argv[0]) == "node":
        if os.getenv("MEVEDEL_TEST_NODE_FAILURE"):
            print("PRIVATE-DIAGNOSTIC-DO-NOT-FORWARD")
            sys.exit(3)
        print(os.getenv("MEVEDEL_TEST_NODE_VERSION", "v22.4.0"))
    elif os.path.basename(sys.argv[0]) == "claude-agent-acp":
        print("0.86.0")
    else:
        print("2.1.290 (Claude Code)")
elif sys.argv[1:] == ["auth", "status", "--json"]:
    if os.getenv("MEVEDEL_TEST_LOGGED_OUT"):
        # The real CLI exits 1 while still printing valid status JSON.
        print(json.dumps({"loggedIn": False, "authMethod": "none", "apiProvider": "firstParty"}))
        sys.exit(1)
    print(json.dumps({"loggedIn": True,
                      "authMethod": os.getenv("MEVEDEL_TEST_AUTH_METHOD", "claude.ai"),
                      "apiProvider": os.getenv("MEVEDEL_TEST_AUTH_PROVIDER", "firstParty"), "subscriptionType": "max"}))
elif sys.argv[1:] == ["install", "stable"]:
    sys.exit(1 if os.getenv("MEVEDEL_TEST_INSTALL_FAIL") else 0)
elif sys.argv[1:] == ["auth", "login", "--claudeai"]:
    print("https://claude.ai/oauth/authorize?state=fixture-state", flush=True)
    code = sys.stdin.readline().strip()
    sys.exit(0 if code == "fixture-code#fixture-state" else 1)
elif not sys.argv[1:]:
    for line in sys.stdin:
        request = json.loads(line)
        if request.get("method") == "initialize":
            print(json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": {
                "protocolVersion": 1, "agentCapabilities": {"loadSession": True}}}), flush=True)
else:
    sys.exit(2)
