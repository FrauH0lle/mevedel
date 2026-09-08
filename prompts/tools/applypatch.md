Propose a coherent text-file change as one structured patch. Related operations
can be reviewed and applied together.

### When to use `ApplyPatch`

- Create, edit, delete, move, or rename text files.
- Edit session scratch content through a non-bare `local://` address.

### When NOT to use `ApplyPatch`

- Empty files or standalone directory creation: the patch grammar cannot express them.
- Directive Planning remains read-only even for local proposals.
- Re-proposing a rejected change unless the user's feedback asks for a revision.

### How to use `ApplyPatch`

- Paths are absolute or relative to the session working directory. The only
  writable resource family is `local://`; other schemes and bare addresses
  are not patch targets. Keep authored addresses in patch markers.
- Standalone or sticky Plan mode allows only proposals whose every source and
  destination is a non-bare `local://` descendant. Outside Plan mode, local and
  ordinary paths may share one atomic proposal, subject to permission/review.
  Disallowed or malformed targets are denied before materialization.
- Understand the relevant contents before changing them. Unsaved buffer edits
  are rejected; existing user changes must be preserved.
- The basic grammar is:

```text
*** Begin Patch
*** Add File: new-path
+new line
*** Update File: existing-path
@@
 unchanged context
-removed line
+replacement line
*** Delete File: obsolete-path
*** End Patch
```

- Add creates missing parent directories but rejects an existing file. Update
  hunks must locate the old content unambiguously; include useful surrounding
  context. An ambiguous match fails instead of choosing a location. Delete
  removes the entire file. A move uses `*** Update File: old-path` followed by
  `*** Move to: new-path`; source and destination are one operation.
- An Update may contain ordered `@@` hunks. A hunk containing only unchanged
  context is a locator for later hunks. Include a changing hunk unless the Update
  moves the file.
- For unchanged locator hunks, multiple hunks, repeated matches, line/context
  anchors, or move details, first read `mevedel://tools/applypatch.md`. The manual
  defines detailed matching behavior. If unavailable, use only grammar described
  here when it meets the user's requested method; otherwise report the missing
  guidance.
- Results report application/rejection or actionable errors. Do not treat a
  proposed or rejected patch as an applied change.

### Examples of good usage

<example>
- Update a small configuration with unique context:
ApplyPatch(patch="*** Begin Patch
*** Update File: config.py
@@
 API_HOST = 'localhost'
-MAX_RETRIES = 1
+MAX_RETRIES = 3
 TIMEOUT = 10
*** End Patch")
</example>

### Examples of bad usage

<example>
ApplyPatch(patch="*** Begin Patch
*** Add File: config.py
+MAX_RETRIES = 3
*** End Patch")
<reasoning>
If config.py already exists, Add fails. Use Update with its actual content.
</reasoning>
</example>
