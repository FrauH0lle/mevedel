Read Emacs Info manuals: list the available manuals, list one manual's nodes, or
read one node.

### When to use `EmacsManual`

- Read authoritative Emacs, Elisp, or package documentation installed as Info.

### When NOT to use `EmacsManual`

- Finding the section for one symbol: use EmacsDescribe with `aspect="manual"`.

### How to use `EmacsManual`

- Omit both arguments to list manuals. Pass `manual` alone to list its node
  names, then `manual` and an exact `node` to read that node.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
EmacsManual(manual="elisp", node="Sequences Arrays Vectors")
</example>

### Examples of bad usage

<example>
EmacsManual(node="Top")
<reasoning>
A node needs its manual; pass `manual` as well.
</reasoning>
</example>
