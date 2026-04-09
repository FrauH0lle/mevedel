Search the web and return top results with URLs and excerpts.

Returns up to 5 search results from the search engine (typically
DuckDuckGo). Each result includes `:url` and `:excerpt` keys.

Uses the Emacs web browser (eww) - no API key required.

Timeout after 30 seconds per search.

### When to use `WebSearch`

- Finding recent documentation or resources online
- Looking up current information not in training data
- Discovering URLs for packages, libraries, or tools
- Researching error messages or unfamiliar concepts

### When NOT to use `WebSearch`

- Reading known URLs -> use `WebFetch`
- Fetching YouTube transcripts -> use `YouTube`
- Information likely in local codebase -> use `Grep` or delegate
- When offline access is required

### How to use `WebSearch`

- Query can be natural language or keywords
- Returns excerpts, not full page content
- Use `WebFetch` with returned URLs to read full content
- Note: `WebFetch` may not work on JavaScript-heavy sites
- Default returns 5 results, can specify different count

### Examples of good usage

<example>
- Find official documentation:
WebSearch(query="Emacs gptel library documentation")
</example>

<example>
- Research error message:
WebSearch(query="elisp void-function error debugging", count=3)
</example>
