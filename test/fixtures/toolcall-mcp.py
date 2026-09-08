"""Local MCP stdio fixture for the ToolCall adapter boundary."""
import json
import sys

schema = {
    "type": "object",
    "properties": {"payload": {
        "type": "object",
        "properties": {"flag": {"type": "boolean"}, "label": {"type": "string"}},
        "required": ["flag", "label"],
    }},
    "required": ["payload"],
}
for line in sys.stdin:
    request = json.loads(line)
    if "id" not in request:
        continue
    method = request["method"]
    if method == "initialize":
        result = {
            "protocolVersion": request["params"]["protocolVersion"],
            "serverInfo": {"name": "toolcall-fixture", "version": "1"},
            "capabilities": {"tools": {}},
        }
    elif method == "tools/list":
        result = {"tools": [{"name": "Probe", "description": "Echo nested JSON", "inputSchema": schema}]}
    elif method == "tools/call":
        arguments = request["params"]["arguments"]
        failed = arguments["payload"]["label"] == "fail"
        result = {
            "isError": failed,
            "content": [{"type": "text", "text": "Error: fixture failure" if failed else json.dumps(arguments)}],
        }
    else:
        result = {}
    print(json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result}), flush=True)
