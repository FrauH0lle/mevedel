Search the web for URLs and excerpts.

### When to use `WebSearch`

- Discover online sources for current information, documentation, or an unfamiliar
  topic relevant to the task.

### When NOT to use `WebSearch`

- Reading a known URL; WebFetch retrieves its contents.
- Looking for private or project-local facts that an internet search cannot supply.

### How to use `WebSearch`

- Pass a query. Returns formatted text with up to five URLs and excerpts from
  Emacs's configured eww search engine. Excerpts are not full pages; WebFetch can
  retrieve a result that needs closer inspection.
- No API key is required. Each underlying fetch has a 30-second timeout; queued
  searches can take longer. Empty output or a fetch error is not proof that no
  relevant source exists.

### Examples of good usage

<example>
WebSearch(query="Python json official documentation")
</example>

### Examples of bad usage

<example>
WebSearch(query="https://docs.python.org/3/library/json.html") to read that page
<reasoning>
The address is already known. Use WebFetch(url="https://docs.python.org/3/library/json.html").
</reasoning>
</example>
