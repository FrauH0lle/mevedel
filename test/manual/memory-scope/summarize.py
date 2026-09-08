"""Render recorded measurements and a semantic-review sheet; no model grading."""

import json
import statistics
import sys
from collections import defaultdict
from pathlib import Path


directory = Path(sys.argv[1])
groups = defaultdict(list)
rows = []
for path in sorted(directory.glob("*.json")):
    records = json.loads(path.read_text())
    if not isinstance(records, list):
        continue
    for row in records:
        groups[row["model"], row["arm"]].append(row)
        rows.append(row)

print("| Model | Policy | Pairs | Successful requests | Tool calls | Patch errors | "
      "Input (uncached) | Input (cached) | Output tokens | Median pair seconds |")
print("|---|---|---:|---:|---:|---:|---:|---:|---:|---:|")
for (model, arm), cases in sorted(groups.items()):
    phases = [row[phase] for row in cases for phase in ("writer", "reader")]
    calls = [call for phase in phases for call in phase["calls"]]
    successes = sum(phase["status"] == "success" for phase in phases)
    patch_errors = sum(call["tool"] == "ApplyPatch" and call["result"].startswith("Error:")
                       for call in calls)
    tokens = [sum(phase[key] for phase in phases)
              for key in ("input_tokens", "cached_tokens", "output_tokens")]
    seconds = statistics.median(row["writer"]["seconds"] + row["reader"]["seconds"]
                                for row in cases)
    print(f"| {model} | {arm} | {len(cases)} | {successes}/{len(phases)} | "
          f"{len(calls)} | {patch_errors} | {tokens[0]} | {tokens[1]} | {tokens[2]} | {seconds:.1f} |")

print("\n## Semantic review evidence\n")
for row in rows:
    print(f"### {row['model']} / {row['arm']} / {row['case']} / trial {row['trial']}\n")
    print(f"Unrelated plan, peer notes, memory and journal preserved: {row['preserved']}\n")
    for phase in ("writer", "reader"):
        request = row[phase]
        print(f"**{phase.capitalize()} ({request['status']})**\n\n{request['reply']}\n")
        for call in request["calls"]:
            if call["tool"] == "ApplyPatch":
                print(f"Patch result: {call['result']}\n\n```diff\n{call['args'][0]}\n```\n")
    print("**Files changed by writer**\n")
    for file in row["after"]:
        if file not in row["before"]:
            print(f"`{file['path']}`\n\n```text\n{file['content']}\n```\n")
    if "final" in row:
        print("**Files created or changed by reader**\n")
        for file in row["final"]:
            if file not in row["after"]:
                print(f"`{file['path']}`\n\n```text\n{file['content']}\n```\n")
        final_paths = {file["path"] for file in row["final"]}
        for file in row["after"]:
            if file["path"] not in final_paths:
                print(f"Deleted: `{file['path']}`\n")
