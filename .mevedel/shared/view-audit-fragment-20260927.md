# Audit fragment rendered as a tool row — 2026-09-27

Task: investigate the user's screenshot from session
`2026-09-27T16-24-5ca69305960c` without disturbing its running request.

## Confirmed observations

- Screenshot shows a fallback `?` tool row beginning
  `Nlc3MuIEnigJlsbCByZXRye`, immediately before a failed ToolCall.
- This exact text is inside the hidden `provider-tool-batch` audit for Bash
  `call_asiMlIwCl1YsSGfn6riT6U4Q`, not a tool result or assistant prose.
- Comparing saved publications `generation-890d1d8ff3eed282c26f` and
  `generation-eb1807f3800f9b2e93d7` showed only GPTEL_BOUNDS changes and a
  Bash render-data replacement adding 2,300 characters. The latter adds
  captured ERT output to the hidden execution metadata.
- In publication `generation-0ea3cdc9cb40849b6afd`, the following ToolCall form
  starts at zero-based character 309346; the screenshot's base64 fragment
  starts at 307046. The difference is exactly 2,300. Thus the old ToolCall
  position identifies the visible fragment after that metadata update.
- The following ToolCall (`call_4yucedTXOuzjXgfKE7ZJHh4k`) independently failed
  with `Error: Unbound variable: false`, from `:preserve_ui false`.
- Approved read-only live inspection found the fragment correctly tagged
  `gptel=mevedel-hook-audit` and `mevedel-hook-audit=t` in the data buffer.
  The view no longer showed the fragment; it showed the legitimate failed
  ToolCall and curl exit 28 request failure. No redraw/reload was requested.
- Live render tracing was disabled and its trace buffer absent. Source-range
  construction in the loaded runtime uses markers, matching current source.

## Reproduction and fix

The user confirmed the issue appeared after expanding a row. The single-row
progress refresh (`mevedel-view-render--refresh-tool-row-now`) stamped numeric
source bounds onto the replacement row, bypassing the marker-backed source
constructor used by normal rendering.

The regression refreshes a following tool, grows an earlier tool's hidden
metadata through `mevedel-tool-render-data-update`, then calls the public
`mevedel-view-toggle-section`. Before the fix it displays the intervening
base64 audit payload instead of `following-result`. This reproduces the stale
consumer without injecting timing or changing mutation-hook behavior.

The fix constructs marker-backed source bounds for the refreshed row. The
regression also checks that the composer draft remains intact and that the
expanded source uses markers. The documentation now explicitly covers progress
refreshes. No product modules outside the view renderer changed.

Verification:
- Red run: 375/376 passed; the new regression alone failed with visible base64.
  Evidence: `artifact://executions/execution-3zJDYf.log`.
- Fixed run: all 605 renderer, disclosure, stream, and render-data tests passed
  through isolated Eask after `clean elc`.
  Evidence: `artifact://executions/execution-YgPuMo.log`.
- Full compilation: 208 files, no warnings/errors. Compiled files cleaned after.
  Evidence: `artifact://executions/execution-EXOrsW.log`.
- `git diff --check` passed.

No live reload or changes to the affected buffers/request. The running Emacs
still needs the updated renderer loaded before the fix affects it. Some saved
publications were pruned by the active session during the investigation; the
observations above were collected before that pruning.
