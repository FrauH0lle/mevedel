Read a library source file resolved through Emacs load-path.

### When to use `library_source`

- Inspect a library when the relevant behavior spans several definitions.

### When NOT to use `library_source`

- Reading an arbitrary filesystem path or proving which code version is already
  loaded.

### How to use `library_source`

- Pass a simple library name without directory components. The wrapper requires
  resolution inside a local load-path entry; remote entries and escaping paths are
  denied. Returns the resolved file contents. The file can differ from code already
  loaded into Emacs.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
library_source(library="cl-lib")
</example>

### Examples of bad usage

<example>
library_source(library="/tmp/custom.el")
<reasoning>
Arbitrary paths are outside this lookup contract. This tool accepts local load-path library names.
</reasoning>
</example>
