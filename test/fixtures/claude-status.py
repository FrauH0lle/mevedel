#!/usr/bin/env python3
"""Supported Claude CLI status surface, without credentials or model calls."""

import json
import os
import pathlib
import shutil
import sys
import time

if os.path.basename(sys.argv[0]) == "npm":
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
elif sys.argv[1:] == ["auth", "status"]:
    print(json.dumps({"loggedIn": True,
                      "authMethod": os.getenv("MEVEDEL_TEST_AUTH_METHOD", "claude.ai"),
                      "apiProvider": "firstParty", "subscriptionType": "max"}))
else:
    sys.exit(2)
