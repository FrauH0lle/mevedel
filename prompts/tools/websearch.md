Search the web for titled result links and snippets.

### When to use `WebSearch`

- Discover online sources for current information, documentation, or an unfamiliar
  topic relevant to the task.
- Verify a belief that may have changed since training, such as a library's current
  version or API, a price, a law, or recent news. If there is a real chance the fact
  has changed, check it rather than relying on memory.

### When NOT to use `WebSearch`

- Reading a known URL; WebFetch retrieves its contents.
- Looking for private or project-local facts that an internet search cannot supply.

### How to use `WebSearch`

- Pass a query. Returns up to ten numbered DuckDuckGo results, each with title, URL,
  and snippet. Snippets are not full pages; WebFetch can retrieve a result that needs
  closer inspection.
- `allowed_domains` restricts results to those domains and their subdomains;
  `blocked_domains` excludes them. Pass at most one of the two.
- Prefer primary and official sources. When a claim in your answer comes from a
  result, link the page that supports it rather than the search.
- No API key is required. Each search has a 30-second timeout; queued searches can
  take longer. Empty output or a fetch error is not proof that no relevant source
  exists. DuckDuckGo occasionally refuses automated searches; retry later instead
  of rephrasing.

### Examples of good usage

<example>
WebSearch(query="Python json official documentation")
</example>

<example>
WebSearch(query="gptel tool use", allowed_domains=["github.com"])
</example>

### Examples of bad usage

<example>
WebSearch(query="https://docs.python.org/3/library/json.html") to read that page
<reasoning>
The address is already known. Use WebFetch(url="https://docs.python.org/3/library/json.html").
</reasoning>
</example>
