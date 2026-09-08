# ApplyPatch matching and multi-file changes

The description contains the ordinary patch grammar and permission/Plan-mode
boundaries. This manual explains disambiguation and less common operations.
It does not override a rejected proposal, user scope, or permissions.

## Context, anchors, and repeated matches

An Update may contain multiple `@@` hunks, ordered top-to-bottom. Include enough
unchanged context to locate each change uniquely; three lines on either side
are often sufficient. Adjacent hunks should not repeat overlapping context.

When context matches multiple locations, enlarge the hunk or add one anchor:

- `@@ N` uses the line number of the hunk's first line in the most recent Read.
  It only chooses among content-matching locations. A stale number does not
  reject an otherwise unique match and cannot force a content mismatch.
- `@@ context anchor` names a line above the hunk, or a prefix of that line,
  such as an enclosing definition. The hunk starts below the anchor.

```text
ApplyPatch(patch="*** Begin Patch
*** Update File: src/config.py
@@ def load_config
-    data = json.load(open(path))
+    with open(path) as fh:
+        data = json.load(fh)
     return validate(data)
*** End Patch")
```

Each hunk takes at most one anchor and must contain at least one line. A hunk
made only of unchanged context is a locator: it changes nothing but positions
the hunks after it. An entire Update that changes nothing is rejected.

Matching first prefers exact contents, then tolerates trailing whitespace,
surrounding whitespace, and ASCII/typographic punctuation differences. Multiple
remaining matches are an error. Existing context lines are retained from the
file, not rewritten from the patch. Hunk order can disambiguate later matches.

## Adds, deletes, and moves

Add creates missing parent directories and requires a new target. To replace
an existing file, use an Update with the actual old contents. An Add must have
content; this grammar does not create an empty file or standalone directory.
Delete removes the whole file.

A move uses Update followed by Move to, with optional content hunks:

```text
*** Begin Patch
*** Update File: src/old.py
*** Move to: src/new.py
@@
-NAME = 'old'
+NAME = 'new'
*** End Patch
```

The source and destination form one indivisible operation. Related operations
can share one proposal; keep unrelated changes separate. Working-file descendants
and explicit `memory://ROOT-KEY/RELATIVE-PATH` targets may mix with ordinary paths
outside Plan mode, subject to normal edit permissions and review. Bare, root-only,
and union addresses are invalid targets; other resource families are read-only.
Standalone/sticky Plan mode permits only session-owned `work://` descendants for
every source and destination. `work://shared/...`, memory, and ordinary paths do
not receive that exception. Directive Planning permits no patches.

## Failure and review

An ambiguous/missing match, invalid grammar, existing Add target, or unsaved
buffer edits produces an error rather than a guessed edit. Inspect current
contents and correct the cause. Do not replay a user-rejected proposal unless
their feedback asks for revision. A proposal is not evidence of application;
use the tool's settled result and inspect the resulting change when needed.
