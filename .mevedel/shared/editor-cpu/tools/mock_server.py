# OpenAI-compatible SSE mock for animation measurements.
# Behavior per request comes from CONTROL (JSON): {"hold": seconds, "tool": {...} | null}.
# - A request whose messages already contain a tool result gets a short text answer.
# - Otherwise, after HOLD seconds of silence, it streams the tool call (if any) or a short text.
# The first request body is saved to BODY for schema inspection.
import http.server, json, sys, time, os
PORT = int(sys.argv[1]); CONTROL = sys.argv[2]; BODY = sys.argv[3]

class H(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    def log_message(self, *a): pass
    def do_POST(self):
        raw = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        if not os.path.exists(BODY):
            open(BODY, "wb").write(raw)
        try:
            body = json.loads(raw)
        except Exception:
            body = {}
        ctl = json.load(open(CONTROL))
        messages = body.get("messages", [])
        def text(m):
            c = m.get("content")
            return c if isinstance(c, str) else json.dumps(c)
        last_prompt = max([i for i, m in enumerate(messages)
                           if m.get("role") == "user" and "measure" in text(m)] or [-1])
        has_tool_result = any(m.get("role") == "tool" for m in messages[last_prompt + 1:])
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "close")
        self.end_headers()
        base = {"id": "mock", "object": "chat.completion.chunk", "model": "mock"}
        def send(obj):
            self.wfile.write(("data: " + json.dumps(obj) + "\n\n").encode()); self.wfile.flush()
        if not has_tool_result:
            time.sleep(ctl.get("hold", 0))
        tool = None if has_tool_result else ctl.get("tool")
        if tool:
            send(dict(base, choices=[{"index": 0, "delta": {"role": "assistant", "tool_calls": [
                {"index": 0, "id": "call_mock", "type": "function",
                 "function": {"name": tool["name"], "arguments": json.dumps(tool["args"])}}]},
                "finish_reason": None}]))
            send(dict(base, choices=[{"index": 0, "delta": {}, "finish_reason": "tool_calls"}]))
        else:
            for w in "Done measuring.".split():
                send(dict(base, choices=[{"index": 0, "delta": {"content": w + " "}, "finish_reason": None}]))
            send(dict(base, choices=[{"index": 0, "delta": {}, "finish_reason": "stop"}]))
        send(dict(base, choices=[], usage={"prompt_tokens": 10, "completion_tokens": 3, "total_tokens": 13}))
        self.wfile.write(b"data: [DONE]\n\n"); self.wfile.flush()
        self.close_connection = True

http.server.ThreadingHTTPServer(("127.0.0.1", PORT), H).serve_forever()
