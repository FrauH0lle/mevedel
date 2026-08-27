Apply one coherent filesystem change with a structured patch. Put every
related file operation in one call. Paths are relative to the session working
directory unless absolute.

### When to use `ApplyPatch`

- Creating, editing, deleting, moving, or renaming text files
- Batching the related file operations of one coherent change into a
  single atomic proposal
- Editing the `local://` writable session scratchpad; it is the only resource
  family writable by ApplyPatch and keeps working notes outside workspace
  source files

### When NOT to use `ApplyPatch`

- Creating an empty file or a directory; neither is expressible in a
  patch -> use `Bash`
- Directive Planning remains read-only: do not call `ApplyPatch` there, even
  for all-local proposals
- Re-proposing a rejected change, unless the user's feedback asks for a
  revision
- Reading files -> use `Read`

### How to use `ApplyPatch`

Resource addresses
- File operands may use ordinary paths or canonical `local://` addresses.
- `local://notes.md` names session scratch content; it is the only resource
  family writable by ApplyPatch. Bare addresses and other resource schemes,
  including `mevedel://`, are not patch targets.
- Keep authored addresses in patch markers. ApplyPatch resolves them once,
  keeps local and ordinary operations in one atomic proposal, and presents
  the authored address in review and results.

Plan-mode boundary
- Standalone or sticky Plan mode permits only proposals whose every source and
  destination target is a non-bare `local://` descendant.
- Ordinary paths, mixed local/ordinary proposals, other-scheme addresses, and
  malformed or bare endpoints are denied before materialization.

Outside Plan mode, mixed local and ordinary operations remain one atomic
proposal.

The argument must use this grammar:

```text
*** Begin Patch
*** Add File: path
+every added line starts with +
*** Update File: path
@@ optional context anchor or line number
 unchanged context starts with one space
-removed line
+added line
*** Delete File: path
*** Update File: old-path
*** Move to: new-path
@@
-old content
+new content
*** End Patch
```

An Update may contain multiple `@@` hunks. By default include three unchanged
context lines above and below each change; when consecutive changes sit fewer
than three lines apart, do not repeat the overlapping context. If three lines
of context cannot uniquely locate the snippet, anchor the hunk with `@@ N`,
where N is the line number its first line carried in your most recent Read of
that file. A line number only chooses among locations whose content already
matches, so a stale N is harmless and never rejects a hunk on its own.
Alternatively add a `@@ context anchor` naming a line above the hunk or a
prefix of one, such as the enclosing definition's first line, or enlarge the
hunk. Each hunk takes at most one `@@` anchor.

A hunk made only of context lines is a locator: it matches its lines and
changes nothing, pinning where the hunks after it apply. Every hunk must
contain at least one line, and an Update whose hunks change nothing is
rejected as a whole.

Matching tolerates trailing whitespace, surrounding whitespace, and
ASCII-vs-typographic punctuation differences, in that order of preference,
but a hunk that matches more than one location is rejected: anchor it with
`@@ N`, enlarge it, or add a context anchor. Order hunks top-to-bottom within
a file; hunk order can disambiguate repeated patterns. Applied context lines
are taken from the file, never rewritten from the patch.

Add creates missing parent directories and cannot target an existing file;
rewrite an existing file with one full-file Update hunk instead. Delete
removes the whole file. Move is one indivisible source/destination operation
and may also contain update hunks. Do not use shell commands for file edits.

### Examples of good usage

<example>
- Edit one function with an anchored hunk; the anchor names a line above
  the hunk, so the hunk starts below it:
ApplyPatch(patch="*** Begin Patch
*** Update File: src/config.py
@@ def load_config
-    data = json.load(open(path))
+    with open(path) as fh:
+        data = json.load(fh)
     return validate(data)
*** End Patch")
</example>

<example>
- One coherent change touching two files:
ApplyPatch(patch="*** Begin Patch
*** Add File: src/limits.py
+MAX_RETRIES = 3
*** Update File: src/client.py
@@
 import time
+from limits import MAX_RETRIES
*** End Patch")
</example>

### Examples of bad usage

<example>
ApplyPatch(patch="*** Begin Patch
*** Add File: src/client.py
+...entire rewritten file...
*** End Patch")
<reasoning>
Add cannot target an existing file. Rewrite an existing file with one
full-file Update hunk instead.
</reasoning>
</example>

<example>
ApplyPatch(patch="*** Begin Patch
*** Update File: src/util.py
@@
-    return None
+    return default
*** End Patch")
<reasoning>
No surrounding context: a one-line hunk that matches several locations
is rejected. Include three context lines or an @@ anchor naming the
enclosing definition.
</reasoning>
</example>
