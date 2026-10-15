"""Exercise project hook decisions through their JSON CLI, without tool execution."""

import json
import subprocess
import sys
import unittest
from pathlib import Path


POLICY = Path(__file__).resolve().parents[1] / ".mevedel/hooks/policy.py"


class ProjectHookPolicyTests(unittest.TestCase):
    def test_configured_handlers_accept_event_payload(self):
        config = json.loads((POLICY.parent.parent / "hooks.json").read_text())
        self.assertEqual({"PreToolUse"}, set(config["hooks"]))
        self.assertEqual(1, len(config["hooks"]["PreToolUse"]))
        for event, groups in config["hooks"].items():
            for group in groups:
                self.assertEqual("ApplyPatch", group["matcher"])
                self.assertEqual(1, len(group["hooks"]))
                for handler in group["hooks"]:
                    with self.subTest(event=event, command=handler["command"]):
                        command = handler["command"].split()
                        self.assertEqual(
                            ["python3", ".mevedel/hooks/policy.py"], command[:-1]
                        )
                        self.assertEqual("generated-file-guard", command[-1])
                        self.assertTrue(handler["failClosed"])
                        self.decision(command[-1], {})

    def decision(self, hook, inputs):
        result = subprocess.run(
            [sys.executable, "-B", str(POLICY), hook],
            input=json.dumps({"tool_input": inputs}),
            text=True, capture_output=True, check=True,
        )
        self.assertEqual("", result.stderr)
        return json.loads(result.stdout) if result.stdout.strip() else None

    def test_checks_every_patch_operation_and_both_rename_paths(self):
        safe = "*** Add File: source.el\n+source\n"
        for operation in (
            "*** Add File: generated.elc\n+bytes\n",
            "*** Update File: generated.elc\n@@\n-old\n+new\n",
            "*** Delete File: generated.elc\n",
            "*** Update File: source.el\n*** Move to: generated.elc\n@@\n-old\n+new\n",
            "*** Update File: generated.elc\n*** Move to: source.el\n@@\n-old\n+new\n",
            "*** Delete File: .mevedel/sessions/session/session.meta.el\n",
            "*** Delete File: /tmp/project/.mevedel/sessions/session/data\n",
            "*** Delete File: .mevedel/other/../sessions/session/data\n",
            "  *** Delete File: generated.elc  \r\n",
        ):
            with self.subTest(operation=operation):
                decision = self.decision(
                    "generated-file-guard",
                    {"patch": "*** Begin Patch\n" + safe + operation + "*** End Patch\n"},
                )
                self.assertIsNotNone(decision)
                self.assertEqual("deny", decision["permissionDecision"])

    def test_allows_source_edits_and_header_like_added_content(self):
        for operation in (
            "*** Add File: source.el\n+source\n",
            "*** Update File: source.el\n*** Move to: new.el\n@@\n-old\n+new\n",
            "*** Add File: example.txt\n+*** Delete File: generated.elc\n",
            "*** Delete File: .mevedel/sessions/../source.el\n",
        ):
            with self.subTest(operation=operation):
                self.assertIsNone(self.decision(
                    "generated-file-guard",
                    {"patch": "*** Begin Patch\n" + operation + "*** End Patch\n"},
                ))


if __name__ == "__main__":
    unittest.main()
