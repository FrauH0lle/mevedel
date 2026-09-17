"""Validate raw and pretty gptel logs through the public analyzer CLI."""

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ANALYZER = Path(__file__).resolve().parents[1] / "scripts/analyze-gptel-log.py"


def entry(kind, body, pretty=False):
    marker = {"gptel": kind, "timestamp": "2026-09-17 12:00:00"}
    indent = 2 if pretty else None
    return json.dumps(marker, indent=indent) + "\n" + json.dumps(body, indent=indent) + "\n"


class LogAnalyzerTests(unittest.TestCase):
    def analyze(self, text, *flags):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "synthetic.log"
            path.write_text(text)
            result = subprocess.run(
                [sys.executable, "-B", str(ANALYZER), str(path), *flags],
                text=True, capture_output=True, check=True,
            )
        self.assertEqual("", result.stderr)
        return json.loads(result.stdout) if "--json" in flags else result.stdout

    def test_reads_raw_and_pretty_request_response_pairs(self):
        for pretty in (False, True):
            for request in (
                {"messages": [{"role": "system", "content": "Repository worker"},
                              {"role": "user", "content": "Inspect the changes"}], "stream": False},
                {"instructions": "Repository worker",
                 "input": [{"role": "user", "content": "Inspect the changes"}], "stream": False},
            ):
                with self.subTest(pretty=pretty, request=request):
                    text = entry("request body", request, pretty)
                    text += entry("response body", {
                        "output": [{"type": "message", "content": [{"text": "Reviewed"}]}],
                        "usage": {"input_tokens": 12, "output_tokens": 3},
                    }, pretty)
                    events = self.analyze(text, "--json")
                    self.assertEqual(["request", "response"], [event["type"] for event in events])
                    self.assertEqual("worker", events[0]["agent"])
                    self.assertFalse(events[0]["stream"])
                    self.assertEqual("Inspect the changes", events[0]["last_user"])
                    self.assertEqual("Reviewed", events[1]["content_preview"])
                    self.assertEqual(12, events[1]["prompt_tokens"])
                    self.assertEqual(3, events[1]["completion_tokens"])

    def test_stream_completions_stay_between_requests(self):
        for stream in (
            {"choices": [{"finish_reason": "stop"}], "usage": {"prompt_tokens": 4, "completion_tokens": 2}},
            {"type": "response.completed", "response": {"usage": {"input_tokens": 4, "output_tokens": 2}}},
        ):
            for pretty in (False, True):
                with self.subTest(stream=stream, pretty=pretty):
                    first = entry("request body", {"messages": [{"role": "user", "content": "First"}]}, pretty)
                    second = entry("request body", {"messages": [{"role": "user", "content": "Second"}]}, pretty)
                    text = first + "data: " + json.dumps(stream) + "\n\n" + second
                    events = self.analyze(text, "--json")
                    self.assertEqual(["request", "stream_finish", "request"],
                                     [event["type"] for event in events])
                    self.assertEqual(sorted(event["line"] for event in events),
                                     [event["line"] for event in events])
                    self.assertEqual(first.count("\n") + 1, events[1]["line"])
                    self.assertEqual(4, events[1]["prompt_tokens"])
                    trace = self.analyze(text, "--verbose")
                    self.assertLess(trace.index("First"), trace.index("[stream]"))
                    self.assertLess(trace.index("[stream]"), trace.index("Second"))

    def test_streamed_response_content_tools_and_errors_keep_log_order(self):
        text = entry("request body", {"input": [{"role": "user", "content": "Inspect"}]})
        for event in (
            {"type": "response.output_item.done", "item": {"type": "function_call", "name": "Read", "arguments": "{\"file_path\":\"a.el\"}"}},
            {"type": "response.output_item.done", "item": {"type": "message", "content": [{"text": "Found it"}]}},
            {"type": "response.completed", "response": {}},
            {"type": "response.failed", "response": {"error": {"code": "unavailable", "message": "Try later"}}},
        ):
            text += "data: " + json.dumps(event) + "\n"
        text += entry("request body", {"input": []})
        events = self.analyze(text, "--json")
        self.assertEqual(["request", "stream_finish", "error", "request"], [event["type"] for event in events])
        self.assertEqual(["Read a.el"], events[1]["tool_calls"])
        self.assertEqual("Found it", events[1]["content_preview"])
        self.assertEqual("unavailable", events[2]["error_code"])
        self.assertGreater(events[2]["line"], events[1]["line"])
        self.assertTrue(all(not key.startswith("_") for event in events for key in event))

    def test_missing_body_does_not_consume_the_next_marker(self):
        for malformed in ("", "{not JSON}\n"):
            with self.subTest(malformed=malformed):
                text = json.dumps({"gptel": "request body"}) + "\n" + malformed
                text += entry("response headers", "HTTP/2 503 Service Unavailable\r\n")
                text += entry("response body", {"detail": "Try later"})
                events = self.analyze(text, "--json")
                self.assertEqual(["error"], [event["type"] for event in events])
                self.assertEqual("503", events[0]["error_code"])
                self.assertEqual("Try later", events[0]["error_message"])


if __name__ == "__main__":
    unittest.main()
