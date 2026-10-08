"""ACP peer streaming prose at a set rate, for CPU measurements.

A copy of test/fixtures/acp-agent.py.  When the session's _meta has
streamRate, a prompt streams numbered words at that many chunks a second
for streamSeconds, writes the count to streamCountPath and ends the turn."""

import json
import os
import sys
import subprocess
import time
import traceback


def reply(request_id, result):
    print(json.dumps({"jsonrpc": "2.0", "id": request_id, "result": result}), flush=True)


def ask(method, params):
    """Send an agent->client request and return the client's response."""
    print(json.dumps({"jsonrpc": "2.0", "id": method, "method": method, "params": params}), flush=True)
    return json.loads(sys.stdin.readline())


def chunk(text, session=None):
    session = session or session_id
    print(json.dumps({"jsonrpc": "2.0", "method": "session/update", "params": {
        "sessionId": session, "update": {"sessionUpdate": "agent_message_chunk",
                                         "content": {"type": "text", "text": text}}}}), flush=True)


def compact():
    for update in compaction_events:
        print(json.dumps({"jsonrpc": "2.0", "method": "session/update", "params": {
            "sessionId": session_id, "update": update}}), flush=True)


PRODUCER = """
import json, sys, time
session, rate, seconds = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
deadline = time.monotonic() + seconds
count = 0
while time.monotonic() < deadline:
    count += 1
    text = "word%d" % count + ("\\n\\n" if count % 10 == 0 else " ")
    print(json.dumps({"jsonrpc": "2.0", "method": "session/update", "params": {
        "sessionId": session, "update": {"sessionUpdate": "agent_message_chunk",
                                         "content": {"type": "text", "text": text}}}}), flush=True)
    time.sleep(1.0 / rate)
"""

session_id = "fixture-session"
stream_rate = None
stream_seconds = 10
stream_count_path = None
pending = None
mcp_servers = []
hook_command = None
pre_tool_hook = False
prompt_response = {"stopReason": "end_turn"}
cancel_response = {"stopReason": "cancelled"}
sdk_while_waiting = []
response_text = None
echo_all_text = False
tool_batches = []
hook_acknowledgement = True
after_hook_tool = None
prompt_acknowledgement = True
compact_before_batch = None
compact_acknowledgement = True
crash_after_tool = False
suppress_response_on_stop = False
expected_image = None
compaction_events = []
compaction_before_batch = None
client_compaction = False
sdk_before_batches = []
sdk_after_batches = []
continuation_prompts = []
prompt_number = 0
session_info = {}
config_behavior = None
applied = {}
inspection = None


def sdk_messages(batches, index):
    if index < len(batches):
        for message in batches[index]:
            print(json.dumps({"jsonrpc": "2.0", "method": "_claude/sdkMessage", "params": {
                "sessionId": session_id, "message": message}}), flush=True)


for line in sys.stdin:
    message = json.loads(line)
    request_id = message.get("id")
    try:
        method = message.get("method")
        params = message.get("params", {})
        if method == "initialize":
            client_compaction = isinstance(params.get("clientCapabilities", {}).get("session", {}).get("compaction"), dict)
            reply(request_id, {"protocolVersion": 1, "agentCapabilities": {
                "loadSession": True, "promptCapabilities": {"image": not os.getenv("MEVEDEL_ACP_TEST_NO_IMAGES")},
                "sessionCapabilities": {"resume": {}}}})
        elif method in ("session/new", "session/resume", "session/load"):
            session_id = params.get("_meta", {}).get("fixtureSessionId", "fixture-session")
            if params.get("sessionId", session_id) != session_id:
                print(json.dumps({"jsonrpc": "2.0", "id": request_id,
                                  "error": {"code": -32000, "message": "Missing history"}}), flush=True)
            else:
                # Real adapters log routine startup progress on stderr.
                print("[session/create] sessionId=%s phase=register" % session_id, file=sys.stderr, flush=True)
                mcp_servers = params.get("mcpServers", [])
                hook_command = params.get("_meta", {}).get("hookCommand")
                pre_tool_hook = params.get("_meta", {}).get("preToolHook", False)
                prompt_response = params.get("_meta", {}).get("promptResponse", {"stopReason": "end_turn"})
                cancel_response = params.get("_meta", {}).get("cancelResponse", {"stopReason": "cancelled"})
                sdk_while_waiting = params.get("_meta", {}).get("sdkWhileWaiting", [])
                response_text = params.get("_meta", {}).get("responseText")
                stream_rate = params.get("_meta", {}).get("streamRate")
                stream_seconds = params.get("_meta", {}).get("streamSeconds", 10)
                stream_count_path = params.get("_meta", {}).get("streamCountPath")
                echo_all_text = params.get("_meta", {}).get("echoAllText", False)
                tool_batches = params.get("_meta", {}).get("toolBatches", [])
                sdk_before_batches = params.get("_meta", {}).get("sdkBeforeBatches", [])
                sdk_after_batches = params.get("_meta", {}).get("sdkAfterBatches", [])
                continuation_prompts = params.get("_meta", {}).get("continuationPrompts", [])
                hook_acknowledgement = params.get("_meta", {}).get("hookAcknowledgement", True)
                after_hook_tool = params.get("_meta", {}).get("afterHookTool")
                crash_after_tool = params.get("_meta", {}).get("crashAfterTool", False)
                suppress_response_on_stop = params.get("_meta", {}).get("suppressResponseOnStop", False)
                expected_image = params.get("_meta", {}).get("expectedImage")
                compact_before_batch = params.get("_meta", {}).get("compactBeforeBatch")
                compact_acknowledgement = params.get("_meta", {}).get("compactAcknowledgement", hook_acknowledgement)
                prompt_acknowledgement = params.get("_meta", {}).get("promptAcknowledgement", True)
                compaction_events = params.get("_meta", {}).get("compactionEvents", [])
                compaction_before_batch = params.get("_meta", {}).get("compactionBeforeBatch")
                if compaction_events and not client_compaction:
                    print(json.dumps({"jsonrpc": "2.0", "id": request_id,
                                      "error": {"code": -32000, "message": "Compaction capability was not advertised"}}), flush=True)
                    continue
                session_info = params.get("_meta", {}).get("sessionInfo", {})
                config_behavior = params.get("_meta", {}).get("configBehavior")
                inspection = params.get("_meta", {}).get("inspection")
                def advertise():
                    commands = [] if inspection.get("missing") else [{"name": "usage", "description": "Subscription usage"}]
                    print(json.dumps({"jsonrpc": "2.0", "method": "session/update", "params": {
                        "sessionId": session_id, "update": {
                            "sessionUpdate": "available_commands_update", "availableCommands": commands}}}), flush=True)
                if inspection and inspection.get("advertise") == "before":
                    advertise()
                reply(request_id, {**session_info, "sessionId": session_id})
                if inspection and inspection.get("advertise") == "after":
                    time.sleep(0.02)
                    advertise()
        elif method == "session/set_config_option":
            option = next(row for row in session_info["configOptions"] if row["id"] == params["configId"])
            assert params["value"] in [row["value"] for row in option["options"]]
            if config_behavior == "wait":
                continue
            if config_behavior == "error":
                print(json.dumps({"jsonrpc": "2.0", "id": request_id,
                                  "error": {"code": -32602, "message": "Fixture config rejected"}}), flush=True)
                continue
            if config_behavior != "mismatch":
                option["currentValue"] = params["value"]
                applied[params["configId"]] = params["value"]
            reply(request_id, session_info)
        elif method == "session/prompt":
            if inspection:
                with open(inspection["promptLog"], "a", encoding="utf-8") as log:
                    log.write(json.dumps(params) + "\n")
                assert not mcp_servers
                assert params["prompt"] == [{"type": "text", "text": "/usage"}]
                if inspection.get("wait"):
                    pending = request_id
                    continue
            if prompt_number:
                script = continuation_prompts[prompt_number - 1] if prompt_number <= len(continuation_prompts) else {}
                tool_batches = script.get("toolBatches", [])
                sdk_before_batches = script.get("sdkBeforeBatches", [])
                sdk_after_batches = script.get("sdkAfterBatches", [])
                compact_before_batch = script.get("compactBeforeBatch")
                compaction_before_batch = None
                compaction_events = []
                prompt_acknowledgement = script.get("promptAcknowledgement", True)
                prompt_response = script.get("promptResponse", {"stopReason": "end_turn"})
                response_text = script.get("responseText", "continued after full context")
            prompt_number += 1
            prompt = params["prompt"][0]["text"]
            if compaction_before_batch is None:
                compact()
            if prompt_acknowledgement:
                receipt = {"jsonrpc": "2.0", "method": "_claude/sdkMessage", "params": {
                    "sessionId": session_id, "message": {
                        "type": "user", "uuid": "fixture-user", "parent_tool_use_id": None,
                        "message": {"role": "user", "content": [
                            {"type": "image", "source": {"type": "base64", "data": block["data"],
                                                            "media_type": block["mimeType"]}}
                            if block["type"] == "image" else block for block in params["prompt"]]}}}}
                if prompt_acknowledgement == "foreign":
                    receipt["params"]["sessionId"] = "another-session"
                elif prompt_acknowledgement == "mismatch":
                    receipt["params"]["message"]["message"]["content"] = [{"type": "text", "text": "truncated"}]
                elif prompt_acknowledgement == "nested":
                    receipt["params"]["message"]["parent_tool_use_id"] = "nested-agent"
                elif prompt_acknowledgement == "image-mismatch":
                    for block in receipt["params"]["message"]["message"]["content"]:
                        if block["type"] == "image":
                            block["source"]["data"] = "truncated"
                if prompt_acknowledgement != "late":
                    # Receipts and terminal replies are sent twice throughout:
                    # observation and settlement must be idempotent.
                    print(json.dumps(receipt), flush=True)
                    print(json.dumps(receipt), flush=True)
            if expected_image:
                images = [block for block in params["prompt"] if block["type"] == "image"]
                if images == [{"type": "image", "data": expected_image, "mimeType": "image/png"}]:
                    chunk("Image accepted")
                    reply(request_id, {"stopReason": "end_turn"})
                else:
                    print(json.dumps({"jsonrpc": "2.0", "id": request_id,
                                      "error": {"code": -32602, "message": "Image payload mismatch"}}), flush=True)
            elif prompt == "report-effort":
                chunk(applied.get("effort", "unset") + (" mode:" + applied["mode"] if "mode" in applied else ""))
                reply(request_id, {"stopReason": "end_turn"})
            elif prompt == "crash":
                print("fixture crash: native binary missing", file=sys.stderr, flush=True)
                sys.exit(7)
            elif prompt == "ask-permission":
                permission = ask("session/request_permission", {
                    "sessionId": session_id, "toolCall": {"toolCallId": "native-1"},
                    "options": [{"optionId": "allow", "name": "Allow", "kind": "allow_once"}]})
                read = ask("fs/read_text_file", {"sessionId": session_id, "path": "/etc/hostname"})
                chunk(json.dumps({"permission": permission.get("result"), "fs": read.get("error")}))
                reply(request_id, {"stopReason": "end_turn"})
            elif prompt == "wait":
                pending = request_id
                sdk_messages([sdk_while_waiting], 0)
                chunk("waiting")
                if prompt_acknowledgement == "late":
                    # Written while the test keeps Emacs from reading, so the
                    # receipt races a tool call the test reads first.
                    time.sleep(0.2)
                    print(json.dumps(receipt), flush=True)
            elif prompt == "wait-silent":
                pending = request_id
            elif tool_batches:
                config = mcp_servers[0]
                with subprocess.Popen([config["command"], *config["args"]],
                                      stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                      text=True) as bridge:
                    def call(value):
                        bridge.stdin.write(json.dumps(value) + "\n")
                        bridge.stdin.flush()
                        return json.loads(bridge.stdout.readline())
                    call({"jsonrpc": "2.0", "id": 1, "method": "initialize"})
                    bridge.stdin.write(json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized"}) + "\n")
                    next_id = 2
                    stopped = False
                    for batch_index, batch in enumerate(tool_batches):
                        sdk_messages(sdk_before_batches, batch_index)
                        if batch_index:
                            chunk("Continuing to the next batch.\n")
                        if compaction_before_batch == batch_index:
                            compact()
                        if compact_before_batch == batch_index and hook_command:
                            output = subprocess.run(hook_command, shell=True, check=True,
                                                    input=json.dumps({"hook_event_name": "SessionStart", "source": "compact"}),
                                                    capture_output=True, text=True, timeout=5)
                            if compact_acknowledgement:
                                receipt = {"jsonrpc": "2.0", "method": "_claude/sdkMessage", "params": {
                                    "sessionId": session_id, "message": {
                                        "type": "system", "subtype": "hook_response", "hook_event": "SessionStart",
                                        "outcome": "success", "exit_code": 0, "stdout": output.stdout}}}
                                print(json.dumps(receipt), flush=True)
                                print(json.dumps(receipt), flush=True)
                            # The real CLI continues after SessionStart(compact),
                            # even when this hook returns continue:false.
                        for tool in batch:
                            if hook_command and pre_tool_hook:
                                output = subprocess.run(hook_command, shell=True, check=True,
                                                        input=json.dumps({"hook_event_name": "PreToolUse",
                                                                          "tool_name": tool["name"]}),
                                                        capture_output=True, text=True, timeout=5)
                                decision = json.loads(output.stdout)
                                if (decision.get("continue") is False
                                        and decision.get("hookSpecificOutput", {}).get("permissionDecision") == "deny"):
                                    stopped = True
                                    break
                            call({"jsonrpc": "2.0", "id": next_id, "method": "tools/call",
                                  "params": {"name": tool["name"], "arguments": tool["args"],
                                             "_meta": {"claudecode/toolUseId": tool["id"]}}})
                            next_id += 1
                            if crash_after_tool:
                                sys.exit(7)
                        if stopped:
                            break
                        sdk_messages(sdk_after_batches, batch_index)
                        if hook_command and batch:
                            output = subprocess.run(hook_command, shell=True, check=True,
                                                    input=json.dumps({"hook_event_name": "PostToolBatch"}),
                                                    capture_output=True, text=True, timeout=5)
                            # A model call racing the hook receipt; Claude makes
                            # none after the hook stops the turn.
                            if after_hook_tool and json.loads(output.stdout).get("continue") is not False:
                                call({"jsonrpc": "2.0", "id": next_id, "method": "tools/call",
                                      "params": {"name": after_hook_tool["name"],
                                                 "arguments": after_hook_tool["args"],
                                                 "_meta": {"claudecode/toolUseId": "after-hook"}}})
                                next_id += 1
                                after_hook_tool = None
                            if hook_acknowledgement:
                                receipt = {"jsonrpc": "2.0", "method": "_claude/sdkMessage",
                                           "params": {"sessionId": session_id, "message": {
                                               "type": "system", "subtype": "hook_response",
                                               "hook_event": "PostToolBatch", "outcome": "success",
                                               "exit_code": 0, "stdout": output.stdout}}}
                                if hook_acknowledgement == "foreign":
                                    receipt["params"]["sessionId"] = "another-session"
                                elif hook_acknowledgement == "mismatch":
                                    receipt["params"]["message"]["stdout"] = json.dumps({
                                        "hookSpecificOutput": {"hookEventName": "PostToolBatch",
                                                               "additionalContext": "truncated"}})
                                elif hook_acknowledgement == "error":
                                    receipt["params"]["message"]["outcome"] = "error"
                                elif hook_acknowledgement == "malformed":
                                    receipt["params"]["message"]["stdout"] = "not JSON"
                                print(json.dumps(receipt), flush=True)
                                print(json.dumps(receipt), flush=True)
                            if json.loads(output.stdout).get("continue") is False:
                                stopped = True
                                break
                    bridge.stdin.close()
                    bridge.wait(timeout=5)
                if not (stopped and suppress_response_on_stop):
                    chunk(response_text or "workload complete")
                reply(request_id, prompt_response)
                reply(request_id, prompt_response)
            elif prompt.startswith("read:"):
                config = mcp_servers[0]
                with subprocess.Popen([config["command"], *config["args"]],
                                      stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                      text=True) as bridge:
                    def send(value):
                        bridge.stdin.write(json.dumps(value) + "\n")
                        bridge.stdin.flush()
                    send({"jsonrpc": "2.0", "id": 1, "method": "initialize"})
                    json.loads(bridge.stdout.readline())
                    send({"jsonrpc": "2.0", "method": "notifications/initialized"})
                    send({"jsonrpc": "2.0", "id": 2, "method": "tools/call",
                          "params": {"name": "Read", "arguments": {"file_path": prompt[5:]},
                                     "_meta": {"fixtureToolId": "fixture-read"}}})
                    result = json.loads(bridge.stdout.readline())["result"]
                    bridge.stdin.close()
                    bridge.wait(timeout=5)
                if hook_command:
                    hook_reply = subprocess.run(hook_command, shell=True, check=True,
                                                input=json.dumps({"hook_event_name": "PostToolBatch"}),
                                                capture_output=True, text=True, timeout=5)
                    decision = json.loads(hook_reply.stdout)
                    chunk(decision.get("stopReason", "unexpected continuation"))
                else:
                    chunk("read finished")
                reply(request_id, prompt_response)
                reply(request_id, prompt_response)
            else:
                chunk("foreign", "another-session")
                if stream_rate:
                    # Like Claude's CLI behind its adapter: a separate producer
                    # keeps writing while this relay is paused.
                    producer = subprocess.Popen([sys.executable, "-c", PRODUCER, session_id,
                                                 str(stream_rate), str(stream_seconds)],
                                                stdout=subprocess.PIPE, text=True)
                    count = 0
                    for line in producer.stdout:
                        count += 1
                        sys.stdout.write(line)
                        sys.stdout.flush()
                    producer.wait()
                    if stream_count_path:
                        open(stream_count_path, "w").write(str(count))
                elif response_text is None:
                    if echo_all_text:
                        prompt = "\n".join(block["text"] for block in params["prompt"] if block["type"] == "text")
                    chunk("answer:")
                    chunk(prompt)
                else:
                    chunk(response_text)
                reply(request_id, prompt_response)
                reply(request_id, prompt_response)
        elif method == "session/cancel" and pending is not None:
            reply(pending, cancel_response)
            pending = None
    except Exception:
        # A scripting mistake reaches the test as this request's error,
        # naming the fixture line, instead of looking like a peer crash.
        traceback.print_exc()
        if request_id is not None:
            print(json.dumps({"jsonrpc": "2.0", "id": request_id, "error": {
                "code": -32603, "message": traceback.format_exc()}}), flush=True)
