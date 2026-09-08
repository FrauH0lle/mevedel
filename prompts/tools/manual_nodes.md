List the topic nodes in an Info manual.

### When to use `manual_nodes`

- Find a relevant section or exact node name within a known manual.

### When NOT to use `manual_nodes`

- Reading the full contents of a section.

### How to use `manual_nodes`

- Pass the manual name. Returns a JSON array of node names; missing manuals or
  lookup failures are errors.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
manual_nodes(manual="elisp")
</example>

### Examples of bad usage

<example>
manual_nodes(manual="elisp/Sequences")
<reasoning>
The manual name and node are separate concepts. Use manual="elisp" to discover its nodes.
</reasoning>
</example>
