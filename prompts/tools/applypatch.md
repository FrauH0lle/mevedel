Propose a coherent text-file change as one structured patch. Related operations
can be reviewed and applied together.

### When to use `ApplyPatch`

- Create, edit, delete, move, or rename text files.
- Edit working files through `work://` or curated memory through explicit
  `memory://ROOT-KEY/RELATIVE-PATH` addresses.

### When NOT to use `ApplyPatch`

- Empty files or standalone directory creation: the patch grammar cannot express them.
- Directive Planning remains read-only even for session working files.
- Re-proposing a rejected change unless the user's feedback asks for a revision.

### How to use `ApplyPatch`

- Paths are absolute or relative to the session working directory. Writable
  resources are `work://` descendants and explicit memory file descendants.
  Bare, root-only, and union addresses and other schemes are not patch targets.
  Keep authored addresses in patch markers.
- Standalone or sticky Plan mode permits only session-owned `work://`
  descendants, such as `work://plans/current.md`, for every source/destination.
  Shared, memory, ordinary, malformed, and root-only endpoints are denied before
  materialization. Outside Plan mode, resource and ordinary paths may share one
  atomic proposal, subject to permission/review. `work://shared/...` belongs to
  the workspace and follows normal edit permissions and review.
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
