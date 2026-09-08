Read one Info manual node.

### When to use `manual_node_contents`

- Retrieve an installed manual section relevant to the task.

### When NOT to use `manual_node_contents`

- Fetching a URL or interpreting a manual as current runtime configuration.

### How to use `manual_node_contents`

- Pass a manual name and exact node name. Returns the full node text, including any
  menu links. Missing manuals/nodes report an error; follow relevant links only when
  more detail is needed.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
manual_node_contents(manual_name="elisp", node="Sequences Arrays Vectors")
</example>

### Examples of bad usage

<example>
manual_node_contents(manual_name="elisp")
<reasoning>
A node is required. Discover its name with manual_nodes or a menu link.
</reasoning>
</example>
