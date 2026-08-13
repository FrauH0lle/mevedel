Apply one coherent filesystem change with a Codex-style patch. Put every
related file operation in one call. Paths are relative to the session working
directory unless absolute.

Resource addresses
- File operands may use ordinary paths or canonical `local://` addresses.
- `local://notes.md` names session scratch content; it is the only resource
  family writable by ApplyPatch. Bare addresses and other resource schemes
  are not patch targets.
- Keep authored addresses in patch markers. ApplyPatch resolves them once,
  keeps local and ordinary operations in one atomic proposal, and presents
  the authored address in review and results.

Plan-mode boundary
- Standalone or sticky Plan mode permits only proposals whose every source and
  destination target is a non-bare `local://` descendant.
- Ordinary paths, mixed local/ordinary proposals, other-scheme addresses, and
  malformed or bare endpoints are denied before materialization.
- Directive Planning remains read-only: do not call `ApplyPatch` there, even
  for all-local proposals.

Outside Plan mode, mixed local and ordinary operations remain one atomic
proposal.

The argument must use this grammar:

```text
*** Begin Patch
*** Add File: path
+every added line starts with +
*** Update File: path
@@ optional context anchor
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
of context cannot uniquely locate the snippet, add a distinguishing
`@@ context anchor` naming the enclosing definition, or enlarge the hunk.
Each hunk takes at most one `@@` anchor.

Matching tolerates trailing whitespace, surrounding whitespace, and
ASCII-vs-typographic punctuation differences, in that order of preference,
but a hunk that matches more than one location is rejected: enlarge it or
anchor it. Order hunks top-to-bottom within a file; hunk order can
disambiguate repeated patterns. Applied context lines are taken from the
file, never rewritten from the patch.

Add creates missing parent directories and cannot target an existing file;
rewrite an existing file with one full-file Update hunk instead. Creating an
empty file or directory is not expressible in a patch; use Bash for those.
Delete removes the whole file. Move is one indivisible source/destination
operation and may also contain update hunks. Do not use shell commands for
file edits. Do not propose a rejected change again unless the user's feedback
asks for a revision.
